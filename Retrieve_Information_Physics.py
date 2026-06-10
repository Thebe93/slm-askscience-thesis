import pandas as pd
from datetime import datetime, timezone
import praw
import time
from tqdm import tqdm

# Path to the input file
input_file = r"C:\Users\thebe\OneDrive\Desktop\Corsi Trento\Tesi\Python\Data\Flair_Data.csv"

# Load the dataset
df = pd.read_csv(input_file, low_memory=False)

# Ensure the 'banned' column is consistent (strip whitespace)
df['banned'] = df['banned'].str.strip()

# Ensure the 'created_utc' column is numeric (Unix timestamps)
df['created_utc'] = pd.to_numeric(df['created_utc'], errors='coerce')

# Convert the Unix timestamp to a datetime format
df['datetime'] = pd.to_datetime(df['created_utc'], unit='s', errors='coerce')

# Filter the data for entries from the year 2019 to 2021
filtered_df = df[df['datetime'].dt.year.isin([2019, 2020, 2021])]

# Exclude threads that were banned (where 'banned' == "No")
filtered_df = filtered_df[filtered_df['banned'] == "No"]

# Exclude closed threads (if the 'is_closed' column exists)
if 'is_closed' in df.columns:
    filtered_df = filtered_df[filtered_df['is_closed'] == False]

# Retain only threads with more than 3 comments
if 'num_comments' in df.columns:
    filtered_df = filtered_df[filtered_df['num_comments'] > 3]

# Filter by "Physics" in the link_flair_text column
if 'link_flair_text' in df.columns:
    filtered_df = filtered_df[filtered_df['link_flair_text'] == "Physics"]

# Initialize Reddit API
reddit = praw.Reddit(
    client_id= ###,
    client_secret= ###,
    user_agent= ###
)

# Initialize dummy name mapping
user_to_dummy = {}
dummy_count = 1

# Global variable to track the last API request time
last_request_time = time.time()

# Function to enforce rate-limiting
def enforce_rate_limit():
    global last_request_time
    elapsed_time = time.time() - last_request_time
    if elapsed_time < 0.5:
        time.sleep(0.5 - elapsed_time)
    last_request_time = time.time()

# Retry logic for handling rate limits
def handle_rate_limit(func, *args, retries=3, delay=5, **kwargs):
    for attempt in range(retries):
        try:
            enforce_rate_limit()  # Ensure rate limiting
            return func(*args, **kwargs)
        except praw.exceptions.APIException as e:
            if "RATELIMIT" in str(e):
                print(f"Rate limit hit. Retrying in {delay} seconds...")
                time.sleep(delay)
                delay *= 2  # Exponential backoff
            else:
                raise e
    print(f"Failed after {retries} retries.")
    return None

# Function to check thread validity
def is_valid_thread(url, reddit):
    try:
        submission_id = url.split('/comments/')[1].split('/')[0]
        submission = handle_rate_limit(reddit.submission, submission_id)
        if submission is None or submission.author is None or submission.selftext in ['[deleted]', '[removed]']:
            return False
        return True
    except Exception as e:
        print(f"Error checking thread status for {url}: {e}")
        return False

# Function to get a dummy name for a user
def get_dummy_name(username):
    global dummy_count
    if username not in user_to_dummy:
        user_to_dummy[username] = f"User{dummy_count}"
        dummy_count += 1
    return user_to_dummy[username]

# Function that get metadata for each comments
def get_author_metadata(comment):
    try:
        enforce_rate_limit()
        author = comment.author
        if author:
            karma = getattr(author, 'comment_karma', None)
            account_creation_time = getattr(author, 'created_utc', None)
            account_age_days = (datetime.now(timezone.utc) - datetime.fromtimestamp(account_creation_time, timezone.utc)).days if account_creation_time else None
            dummy_name = get_dummy_name(author.name)
            return {
                "dummy_name": dummy_name,
                "karma": karma,
                "account_age_days": account_age_days
            }
        else:
            return {"dummy_name": "Anonymous", "karma": None, "account_age_days": None}
    except Exception as e:
        print(f"Error fetching author metadata: {e}")
        return {"dummy_name": "Anonymous", "karma": None, "account_age_days": None}

# Function to fetch answers with Thread User and Comment User relationships
def get_answers_with_connections(url, reddit):
    try:
        submission_id = url.split('/comments/')[1].split('/')[0]
        submission = handle_rate_limit(reddit.submission, submission_id)
        submission.comments.replace_more(limit=None)
        comments_data = []
        comment_id_to_dummy = {}

        # Recursive function to process comments
        def process_comment(comment, parent_dummy="Thread"):
            author_metadata = get_author_metadata(comment)
            dummy_name = author_metadata["dummy_name"]
            comment_id_to_dummy[comment.id] = dummy_name

            # Determine "replied_to"
            replied_to = comment_id_to_dummy.get(comment.parent_id.split('_')[1], "Thread") if comment.parent_id.startswith('t1_') else "Thread"

            # Collect data
            comment_time = datetime.fromtimestamp(comment.created_utc, tz=timezone.utc).isoformat()
            distinguish_status = getattr(comment, 'distinguished', None)
            comments_data.append({
                "body": comment.body,
                "upvotes": comment.score,
                "dummy_name": dummy_name,
                "karma": author_metadata["karma"],
                "account_age_days": author_metadata["account_age_days"],
                "timestamp": comment_time,
                "distinguished": distinguish_status,
                "edited": comment.edited if comment.edited else False,
                "replied_to": replied_to
            })

            for reply in getattr(comment, "replies", []):
                process_comment(reply, parent_dummy=dummy_name)

        for top_level_comment in submission.comments:
            process_comment(top_level_comment)

        return submission_id, comments_data
    except Exception as e:
        print(f"Error fetching data for {url}: {e}")
        return None, []

# Main execution
output_file = "questions_with_answers_and_metadata_with_connections_physics.csv"
data = []

# Process each URL
urls = filtered_df['full_link'].tolist()
for url in tqdm(urls, desc="Processing URLs", unit="url"):
    if is_valid_thread(url, reddit):
        submission_id, comments = get_answers_with_connections(url, reddit)
        for comment in comments:
            data.append({
                "id": submission_id,
                "comment": comment["body"],
                "upvotes": comment["upvotes"],
                "dummy_name": comment["dummy_name"],
                "karma": comment["karma"],
                "account_age_days": comment["account_age_days"],
                "time": comment["timestamp"],
                "distinguished": comment["distinguished"],
                "edited": comment["edited"],
                "replied_to": comment["replied_to"]
            })

# Save results to CSV
output_df = pd.DataFrame(data)
output_df.to_csv(output_file, index=False, encoding='utf-8')

print("Processing complete. Results saved to:", output_file)
