# scalareads

A Goodreads API wrapper in Scala.

Load it up and in sbt:

```
run scalareads [--user id] [--book id] [--author id] --devkey string
```

User IDs, author IDs, and book IDs are all integers found on Goodreads. 

When querying for a user, a line will be printed that gives you a book from that user's to-read list which has the highest predicted rating. 
