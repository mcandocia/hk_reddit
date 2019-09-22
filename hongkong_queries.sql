

DROP TABLE IF EXISTS hongkong_subreddit_comment_relationships CASCADE;
CREATE TABLE hongkong_subreddit_comment_relationships AS

WITH
user_subreddit_relation AS
(SELECT author_name AS username, subreddit, count(*) AS user_subreddit_count 
FROM hongkong.comments 
WHERE author_name IS NOT NULL
AND author_name != 'AutoModerator'
AND created >= '2019-03-31'
GROUP BY author_name, subreddit
HAVING count(*) > 2
) 

SELECT t1.subreddit AS subreddit_1, t2.subreddit AS subreddit_2, 
count(*) AS n_relationships,
sum(t1.user_subreddit_count) AS sub_1_count,
sum(t2.user_subreddit_count) AS sub_2_count,
exp(avg(ln(t1.user_subreddit_count::real/t2.user_subreddit_count))) AS average_ratio
FROM
user_subreddit_relation t1
INNER JOIN
user_subreddit_relation t2
ON t1.username=t2.username
AND t1.subreddit < t2.subreddit
GROUP BY t1.subreddit, t2.subreddit
HAVING count(*) > 10
;


DROP TABLE IF EXISTS hongkong_subreddit_thread_relationships CASCADE;
CREATE TABLE hongkong_subreddit_thread_relationships AS

WITH
user_subreddit_relation AS
(SELECT author_name AS username, subreddit, count(*) AS user_subreddit_count 
FROM hongkong.threads
WHERE author_name IS NOT NULL
AND author_name != 'AutoModerator'
AND created >= '2019-03-31'
GROUP BY author_name, subreddit
HAVING count(*) > 0
) 

SELECT t1.subreddit AS subreddit_1, t2.subreddit AS subreddit_2, 
count(*) AS n_relationships,
sum(t1.user_subreddit_count) AS sub_1_count,
sum(t2.user_subreddit_count) AS sub_2_count,
exp(avg(ln(t1.user_subreddit_count::real/t2.user_subreddit_count))) AS average_ratio
FROM
user_subreddit_relation t1
INNER JOIN
user_subreddit_relation t2
ON t1.username=t2.username
AND t1.subreddit < t2.subreddit
GROUP BY t1.subreddit, t2.subreddit
HAVING count(*) > 10
;

SELECT subreddit, count(*) FROM hongkong.comments 
WHERE subreddit IN ('HongKong','Hong_Kong','China','Sino')
AND username IS NOT NULL
AND username != 'AutoModerator'
GROUP BY subreddit;

-- get some node attributes

CREATE OR REPLACE VIEW hongkong.subreddit_comment_relationships_filtered AS 
SELECT * FROM 
  hongkong_subreddit_comment_relationships
  WHERE n_relationships > 150 AND abs(log(average_ratio)) < 2
  OR (subreddit_1 IN ('China','Sino','HongKong','Hong_Kong') AND n_relationships > 2 AND abs(log(average_ratio)) < 5)
  OR (subreddit_2 IN ('China','Sino','HongKong','Hong_Kong') AND n_relationships > 2 AND abs(log(average_ratio)) < 5)
;

CREATE OR REPLACE VIEW hongkong.subreddit_thread_relationships_filtered AS 
SELECT * FROM 
  hongkong_subreddit_thread_relationships
  WHERE n_relationships > 150 AND abs(log(average_ratio)) < 2
  OR (subreddit_1 IN ('China','Sino','HongKong','Hong_Kong') AND n_relationships > 2 AND abs(log(average_ratio)) < 5)
  OR (subreddit_2 IN ('China','Sino','HongKong','Hong_Kong') AND n_relationships > 2 AND abs(log(average_ratio)) < 5)
;


\copy (SELECT subreddit_1 AS Source, subreddit_2 AS Target, n_relationships AS weight, sub_1_count, sub_2_count, average_ratio FROM hongkong_subreddit_comment_relationships) TO '/ntfsl/workspace/hongkong/subreddit_comment_relationships.csv' DELIMITER ',' CSV HEADER;

\copy (SELECT subreddit_1 AS Source, subreddit_2 AS Target, n_relationships AS weight, sub_1_count, sub_2_count, average_ratio FROM hongkong_subreddit_thread_relationships) TO '/ntfsl/workspace/hongkong/subreddit_thread_relationships.csv' DELIMITER ',' CSV HEADER;

CREATE OR REPLACE VIEW hongkong.subreddit_comment_summary AS
SELECT subreddit AS id, subreddit AS label, count(*) AS num_users, sum(n_comments) AS n_comments
FROM 
(SELECT subreddit, author_name, count(*) AS n_comments FROM 
    hongkong.comments 
    WHERE author_name IS NOT NULL
    AND author_name != 'AutoModerator'
    GROUP BY subreddit, author_name
    HAVING count(*) > 2
) t1
WHERE subreddit IN (
    SELECT subreddit_1 FROM hongkong.subreddit_comment_relationships_filtered 
    UNION 
    select subreddit_2 from hongkong.subreddit_comment_relationships_filtered
    )
GROUP BY subreddit;
\copy (SELECT *, CASE WHEN id IN ('China','HongKong','Hong_Kong','Sino') THEN 'focus' ELSE 'other' END AS subreddit_group FROM hongkong.subreddit_comment_summary) TO '/ntfsl/workspace/hongkong/subreddit_comment_summary.csv' DELIMITER ',' CSV HEADER;

CREATE OR REPLACE VIEW hongkong.subreddit_thread_summary AS
SELECT subreddit AS id, subreddit AS label, count(*) AS num_users, sum(n_threads) AS n_threads
FROM 
(SELECT subreddit, author_name, count(*) AS n_threads FROM 
    hongkong.threads
    WHERE author_name IS NOT NULL
    AND author_name != 'AutoModerator'
    GROUP BY subreddit, author_name
) t1
WHERE subreddit IN (
    SELECT subreddit_1 FROM hongkong.subreddit_thread_relationships_filtered 
    UNION 
    select subreddit_2 from hongkong.subreddit_thread_relationships_filtered
    )
GROUP BY subreddit;
\copy (SELECT *, CASE WHEN id IN ('China','HongKong','Hong_Kong','Sino') THEN 'focus' ELSE 'other' END AS subreddit_group FROM hongkong.subreddit_thread_summary) TO '/ntfsl/workspace/hongkong/subreddit_thread_summary.csv' DELIMITER ',' CSV HEADER;

-- look at first-order relationships only
\copy (SELECT subreddit_1 AS Source, subreddit_2 AS Target, n_relationships AS weight, sub_1_count, sub_2_count, average_ratio FROM hongkong.subreddit_comment_relationships_filtered WHERE subreddit_1 IN ('China','HongKong','Hong_Kong','Sino') OR subreddit_2 IN ('China','HongKong','Hong_Kong','Sino')) TO '/ntfsl/workspace/hongkong/subreddit_comment_relationships_reduced.csv' DELIMITER ',' CSV HEADER;
\copy (SELECT subreddit_1 AS Source, subreddit_2 AS Target, n_relationships AS weight, sub_1_count, sub_2_count, average_ratio FROM hongkong.subreddit_thread_relationships_filtered WHERE subreddit_1 IN ('China','HongKong','Hong_Kong','Sino') OR subreddit_2 IN ('China','HongKong','Hong_Kong','Sino')) TO '/ntfsl/workspace/hongkong/subreddit_thread_relationships_reduced.csv' DELIMITER ',' CSV HEADER;


CREATE TABLE gen2020_v1.comment_weights AS
SELECT subreddit, count(DISTINCT author_name) AS n_authors, count(*) AS n_comments
FROM gen2020_v1.comments
GROUP BY subreddit;

CREATE TABLE gen2020_v1.thread_weights AS 
SELECT subreddit, count(DISTINCT author_name) AS n_authors, count(*) AS n_threads 
FROM gen2020_v1.threads
GROUP BY subreddit;


CREATE TABLE hongkong.comment_weights AS
SELECT subreddit, count(DISTINCT author_name) AS n_authors, count(*) AS n_comments
FROM hongkong.comments
GROUP BY subreddit;

CREATE TABLE hongkong.thread_weights AS 
SELECT subreddit, count(DISTINCT author_name) AS n_authors, count(*) AS n_threads 
FROM hongkong.threads
GROUP BY subreddit;

\copy (SELECT * FROM hongkong.comment_weights) TO '/ntfsl/workspace/hongkong/hongkong_comment_weights.csv' DELIMITER ',' CSV HEADER;
\copy (SELECT * FROM hongkong.thread_weights) TO '/ntfsl/workspace/hongkong/hongkong_thread_weights.csv' DELIMITER ',' CSV HEADER;



CREATE TABLE hongkong.subreddit_comment_creddit_scores AS
SELECT subreddit, 
  avg(score) AS score_avg, 
  percentile_cont(0.25) WITHIN GROUP (ORDER BY score) AS score_Q25, 
  percentile_cont(0.5) WITHIN GROUP (ORDER BY score) AS score_Q50, 
  percentile_cont(0.75) WITHIN GROUP (ORDER BY score) AS score_Q75,
  avg(CASE WHEN score < 100 THEN 1 ELSE 0 END) AS score_pct_under_100

FROM 
(
    SELECT username, subreddit, score, probability, cnt FROM 
    (
        SELECT author_name AS username, subreddit, count(*) AS cnt
        FROM hongkong.comments 
        WHERE subreddit IN ('Sino','China','HongKong','Hong_Kong')
        AND created >= '2019-03-31'
        GROUP BY subreddit, author_name
        HAVING count(*) > 2
    ) t1
    LEFT JOIN 
    reddit_models.hongkong_creddit_scores
    USING(username)
) scored
GROUP BY subreddit;


CREATE TABLE hongkong.subreddit_thread_creddit_scores AS
SELECT subreddit, 
  avg(score) AS score_avg, 
  percentile_cont(0.25) WITHIN GROUP (ORDER BY score) AS score_Q25, 
  percentile_cont(0.5) WITHIN GROUP (ORDER BY score) AS score_Q50, 
  percentile_cont(0.75) WITHIN GROUP (ORDER BY score) AS score_Q75,
  avg(CASE WHEN score < 100 THEN 1 ELSE 0 END) AS score_pct_under_100

FROM 
(
    SELECT username, subreddit, score, probability, cnt FROM 
    (
        SELECT author_name AS username, subreddit, count(*) AS cnt
        FROM hongkong.threads
        WHERE subreddit IN ('Sino','China','HongKong','Hong_Kong')
        AND created >= '2019-03-31'
        GROUP BY subreddit, author_name
        HAVING count(*) > 0
    ) t1
    LEFT JOIN 
    reddit_models.hongkong_creddit_scores
    USING(username)
) scored
GROUP BY subreddit;


CREATE TABLE hongkong.subreddit_thread_creddit_scores_all AS
    SELECT username, subreddit, score, probability, cnt FROM 
    (
        SELECT author_name AS username, subreddit, count(*) AS cnt
        FROM hongkong.threads
        WHERE subreddit IN ('Sino','China','HongKong','Hong_Kong')
        AND created >= '2019-03-31'
        GROUP BY subreddit, author_name
        HAVING count(*) > 0
    ) t1
    LEFT JOIN 
    reddit_models.hongkong_creddit_scores
    USING(username);

CREATE TABLE hongkong.subreddit_comment_creddit_scores_all AS
    SELECT username, subreddit, score, probability, cnt FROM 
    (
        SELECT author_name AS username, subreddit, count(*) AS cnt
        FROM hongkong.comments 
        WHERE subreddit IN ('Sino','China','HongKong','Hong_Kong')
        AND created >= '2019-03-31'
        GROUP BY subreddit, author_name
        HAVING count(*) > 0
    ) t1
    LEFT JOIN 
    reddit_models.hongkong_creddit_scores
    USING(username);

\copy (SELECT * FROM hongkong.subreddit_comment_creddit_scores_all) TO '/ntfsl/workspace/hongkong/comment_creddit_scores.csv' DELIMITER ',' CSV HEADER;
\copy (SELECT * FROM hongkong.subreddit_thread_creddit_scores_all) TO '/ntfsl/workspace/hongkong/thread_creddit_scores.csv' DELIMITER ',' CSV HEADER;

\copy (SELECT * FROM hongkong.ngrams) TO '/ntfsl/workspace/hongkong_ngrams.csv' DELIMITER ',' CSV HEADER;