from google.cloud import bigquery


bigquery_client = bigquery.Client()
LIMITED = 'SELECT * FROM [gdelt-bq:full.events_partitioned] LIMIT 100'
query = bigquery_client.run_sync_query(LIMITED)
query.run()