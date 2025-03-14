import boto3

# Setup S3 client
s3_client = boto3.client('s3')

# Specify the S3 bucket and file name
bucket_name = 'your-bucket-name'
file_key = 'path/to/your/file.parquet'

# Querying data using S3 Select
response = s3_client.select_object_content(
    Bucket=bucket_name,
    Key=file_key,
    ExpressionType='SQL',
    Expression="SELECT * FROM S3Object LIMIT 10",  # Modify SQL as needed to sample
    InputSerialization={'Parquet': {}},
    OutputSerialization={'CSV': {}},
)

# Read the response content and process it
for event in response['Payload']:
    if 'Records' in event:
        print(event['Records'].decode('utf-8'))




