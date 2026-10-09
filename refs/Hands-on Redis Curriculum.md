# Hands-on Redis Curriculum

Oct 7, 2026 · @krishnamn

A 4-week, lab-first path from zero to running Redis as an in-memory data store, cache, queue and leaderboard, at about 1 hour a day.

## Roadmap

Each module ends in a lab you must finish before moving on. Run everything yourself in redis-cli first, then in Python.

| Week | Module | Core topics | Proof-of-skill lab |
| --- | --- | --- | --- |
| 1 | 0. Setup | Docker, redis-cli, RedisInsight | Round-trip SET/GET, inspect in GUI |
| 1 | 1. Strings and keys | SET/GET, INCR, TTL, SCAN | Page-view counter with expiry |
| 1 | 2. Hashes, lists, sets | HSET, LPUSH/BRPOP, SADD/SINTER | Session store + job queue |
| 2 | 3. Sorted sets | ZADD, ZRANGE, ZRANK | Leaderboard + sliding-window rate limiter |
| 2 | 4. Caching and eviction | Cache-aside, maxmemory, LRU/LFU | Cache a slow query, measure hit ratio |
| 3 | 5. Streams and pub/sub | XADD, consumer groups, HyperLogLog | Event pipeline with 2 consumers |
| 3 | 6. Durability and atomicity | RDB, AOF, MULTI, pipelines, Lua | Crash-recovery test + atomic stock decrement |
| 4 | 7. Python app | redis-py, connection pools | Cached REST API with rate limiting |
| 4 | 8. High availability | Replication, Sentinel, Cluster | Kill a primary, watch failover |

## Module 0: Setup (30 min)

Run Redis in Docker so you can wipe and rebuild it freely.

```bash
docker run -d --name redis -p 6379:6379 redis:7
docker exec -it redis redis-cli
```

- [ ] Run `PING` and get `PONG`
- [ ] Run `SET hello world`, then `GET hello`
- [ ] Run `INFO memory` and note `used_memory_human`
- [ ] Install RedisInsight and connect to `localhost:6379`
- [ ] Learn `MONITOR` (streams every command; use it to watch your Python code later)

**Mental model to hold:** Redis is a single-threaded server holding typed data structures in RAM. Every command is atomic. Speed comes from memory plus simple operations, so the skill is choosing the right structure per problem.

## Module 1: Strings, keys and TTL (1.5 hrs)

Strings hold text, numbers or bytes up to 512 MB, and they are the base of counters, flags and cached blobs.

```bash
SET user:1:name "Ada"
GET user:1:name
INCR page:home:views
INCRBY page:home:views 10
SET token:abc 1 EX 60 NX     # set only if absent, expire in 60s
TTL token:abc
EXPIRE user:1:name 300
MSET a 1 b 2 c 3
MGET a b c
EXISTS a
DEL a
SCAN 0 MATCH user:* COUNT 100
```

**Key concepts**

- Namespace keys with colons: `object:id:field`
- Use `SCAN`, never `KEYS *`, on real data (KEYS blocks the server)
- `SET ... NX EX` is the building block for locks and idempotency keys
- `INCR` is atomic, so concurrent clients never lose counts

**Lab: page-view counter**

- [ ] Count views per page with `INCR page:<slug>:views`
- [ ] Add a per-day key like `views:2026-10-07:<slug>` that expires after 7 days
- [ ] Verify expiry with `TTL`, then wait and confirm the key vanishes
- [ ] Run 3 `redis-cli` terminals incrementing at once; confirm the total is exact

## Module 2: Hashes, lists and sets (2 hrs)

```bash
# Hashes: objects with fields
HSET user:1 name Ada email ada@x.io plan pro
HGET user:1 name
HGETALL user:1
HINCRBY user:1 logins 1

# Lists: queues and stacks
LPUSH jobs "resize:42"
RPUSH jobs "email:7"
RPOP jobs
BRPOP jobs 5          # blocking pop, waits up to 5s
LRANGE jobs 0 -1

# Sets: unique members, set algebra
SADD tags:post:1 redis cache nosql
SADD tags:post:2 redis streams
SINTER tags:post:1 tags:post:2
SISMEMBER tags:post:1 cache
SCARD tags:post:1
```

**When to use which**

| Need | Structure | Why |
| --- | --- | --- |
| Object with fields | Hash | Update one field without rewriting the whole value |
| FIFO job queue | List | `LPUSH` + `BRPOP` is O(1) and blocking |
| Unique items, membership | Set | O(1) add and lookup |
| Common/different members | Set | `SINTER`, `SUNION`, `SDIFF` |

**Lab: session store and job queue**

- [ ] Store a login session as a hash `session:<id>` with `EXPIRE 1800`, and refresh the TTL on each "request"
- [ ] Build a producer terminal pushing jobs and a worker terminal looping on `BRPOP`
- [ ] Track online users in a set; find users online in both of two rooms with `SINTER`
- [ ] Compare memory of 1,000 small objects stored as JSON strings vs hashes using `MEMORY USAGE`

## Module 3: Sorted sets (2 hrs)

Sorted sets keep unique members ordered by a numeric score, which makes them the most versatile Redis structure.

```bash
ZADD board 1500 alice 1200 bob 1800 carol
ZINCRBY board 50 bob
ZREVRANGE board 0 2 WITHSCORES   # top 3
ZREVRANK board alice              # 0-based rank from top
ZSCORE board carol
ZRANGEBYSCORE board 1000 1600
ZREM board bob
ZCARD board
```

**Sliding-window rate limiter pattern**

```bash
# allow max 5 requests per 60s per user
ZREMRANGEBYSCORE rl:u1 0 <now_ms - 60000>
ZCARD rl:u1                 # if >= 5, reject
ZADD rl:u1 <now_ms> <unique_id>
EXPIRE rl:u1 60
```

**Lab**

- [ ] Build a game leaderboard: add 20 players, show top 5 and a player's rank and neighbors (`ZREVRANK` then `ZREVRANGE`)
- [ ] Implement the rate limiter above and prove the 6th call inside 60s is rejected
- [ ] Use timestamps as scores to build a "recent activity" feed trimmed with `ZREMRANGEBYRANK`
- [ ] Note the race condition between `ZCARD` and `ZADD`; you will fix it with Lua in Module 6

## Module 4: Caching patterns and eviction (2 hrs)

This is the most common production use of Redis. Cache-aside means the app checks Redis first, falls back to the database on a miss, then writes the result back with a TTL.

```bash
CONFIG SET maxmemory 50mb
CONFIG SET maxmemory-policy allkeys-lru
CONFIG GET maxmemory*
INFO stats          # keyspace_hits, keyspace_misses, evicted_keys
OBJECT FREQ mykey   # needs an LFU policy
MEMORY USAGE mykey
```

**Eviction policies to know**

| Policy | Evicts | Use when |
| --- | --- | --- |
| noeviction | Nothing; writes fail when full | Redis is a primary store |
| allkeys-lru | Least recently used, any key | General-purpose cache (best default) |
| allkeys-lfu | Least frequently used, any key | Hot/cold skew |
| volatile-ttl | Soonest-expiring keys with a TTL | Mixed cache and persistent keys |

**Lab: cache a slow query**

- [ ] Write a function that sleeps 500 ms to fake a DB call; wrap it in cache-aside with a 60 s TTL
- [ ] Measure latency on miss vs hit
- [ ] Compute hit ratio from `INFO stats` after 1,000 requests
- [ ] Set `maxmemory 5mb`, flood with unique keys, and watch `evicted_keys` rise
- [ ] Add random jitter to TTLs and explain how it prevents a cache stampede
- [ ] Invalidate on write: delete the cache key when the source row changes

## Module 5: Streams, pub/sub and probabilistic types (2.5 hrs)

Streams are an append-only log with consumer groups, so messages persist and can be replayed. Pub/sub is fire-and-forget: if nobody is listening, the message is lost.

```bash
# Streams
XADD events * type click user 42
XLEN events
XRANGE events - +
XGROUP CREATE events workers $ MKSTREAM
XREADGROUP GROUP workers w1 COUNT 10 BLOCK 5000 STREAMS events >
XACK events workers <message-id>
XPENDING events workers

# Pub/sub
SUBSCRIBE news            # terminal A
PUBLISH news "hello"      # terminal B

# Compact counting
PFADD uniq:visitors u1 u2 u3
PFCOUNT uniq:visitors     # approximate distinct count, ~12 KB max
SETBIT active:2026-10-07 42 1
BITCOUNT active:2026-10-07
```

**Lab: event pipeline**

- [ ] Producer adds 100 events with `XADD`
- [ ] Two consumers in one group split the work; confirm no event is processed twice
- [ ] Kill a consumer before `XACK`, then reclaim its pending message with `XAUTOCLAIM`
- [ ] Repeat the same flow with pub/sub and observe the lost messages when a subscriber is down
- [ ] Track unique daily visitors with `PFADD`, and daily active users with bitmaps

## Module 6: Persistence, transactions, pipelining and Lua (3 hrs)

Memory is volatile, so you choose how much data you can afford to lose. RDB takes periodic snapshots (fast restarts, may lose minutes). AOF logs every write (loses about 1 second with `everysec`). Many setups enable both.

```bash
CONFIG SET appendonly yes
CONFIG SET appendfsync everysec
BGSAVE
LASTSAVE

# Transactions: queued, executed together, no interleaving
MULTI
DECRBY stock:sku1 1
RPUSH orders sku1
EXEC

# Optimistic locking
WATCH balance:1
GET balance:1
MULTI
DECRBY balance:1 20
EXEC               # fails (nil) if balance:1 changed after WATCH

# Lua: atomic read-modify-write
EVAL "local s = tonumber(redis.call('GET', KEYS[1])); if s and s > 0 then return redis.call('DECR', KEYS[1]) else return -1 end" 1 stock:sku1
```

**Lab**

- [ ] Write 100 keys, run `docker restart redis` with no persistence, and observe the data loss
- [ ] Mount a volume (`-v redisdata:/data`), enable AOF, repeat, and confirm data survives
- [ ] Force a crash with `docker kill redis` and check what the last second of writes looks like
- [ ] Cause a `WATCH` conflict from two terminals
- [ ] Rewrite the Module 3 rate limiter as a single Lua script so check-and-add is atomic
- [ ] Benchmark 10,000 `SET`s one by one vs in a pipeline, and compare with `redis-benchmark`

## Module 7: Python app with redis-py (3 hrs)

```bash
pip install redis fastapi uvicorn
```

```python
import json, random, time
import redis

pool = redis.ConnectionPool(host="localhost", port=6379, decode_responses=True)
r = redis.Redis(connection_pool=pool)

def get_user(uid: int):
    key = f"user:{uid}"
    if (hit := r.get(key)):
        return json.loads(hit)
    user = slow_db_lookup(uid)                      # your DB call
    r.set(key, json.dumps(user), ex=300 + random.randint(0, 60))
    return user

# Pipeline: one round trip for many commands
with r.pipeline() as p:
    for i in range(1000):
        p.set(f"k:{i}", i)
    p.execute()
```

**Lab: cached API with rate limiting**

- [ ] Build a FastAPI endpoint `/users/{id}` using cache-aside as above
- [ ] Add a per-IP rate limit (Lua script from Module 6) returning HTTP 429
- [ ] Add `/leaderboard` backed by a sorted set
- [ ] Run `MONITOR` in another terminal and verify the commands your app issues
- [ ] Load-test with `hey` or `wrk` and report p50/p99 latency with and without the cache
- [ ] Write tests against a throwaway Redis container (use `FLUSHDB` between tests)

## Module 8: Replication, Sentinel and Cluster (3 hrs)

Replication copies one primary to read-only replicas. Sentinel watches them and promotes a replica when the primary dies. Cluster shards keys across many primaries using 16,384 hash slots.

| Setup | Solves | Pick it when |
| --- | --- | --- |
| Single node | Nothing; simplest | Dev, small cache you can lose |
| Primary + replicas | Read scaling, a hot copy | Reads dominate |
| Sentinel (3 nodes) | Automatic failover | Data fits one machine, need uptime |
| Cluster | Failover plus more RAM than one machine | Dataset or writes outgrow one node |

```bash
# Replica pointing at a primary
REPLICAOF redis-primary 6379
INFO replication

# Cluster (6 nodes: 3 primaries, 3 replicas)
redis-cli --cluster create host1:7000 host2:7001 host3:7002 host4:7003 host5:7004 host6:7005 --cluster-replicas 1
CLUSTER INFO
CLUSTER KEYSLOT user:1
```

**Lab (Docker Compose)**

- [ ] Start 1 primary and 2 replicas; write on the primary, read on a replica, confirm replicas reject writes
- [ ] Add 3 Sentinels, run `docker stop` on the primary, and watch the promotion in Sentinel logs
- [ ] Connect redis-py through Sentinel and verify your app keeps working after failover
- [ ] Spin up a 6-node cluster and show which node owns `user:1` with `CLUSTER KEYSLOT`
- [ ] Use hash tags like `{user:1}:cart` and `{user:1}:profile` so related keys land on one slot, then try a multi-key command across slots and read the error

## Capstone and self-assessment

Pick one capstone, build it in about a weekend, and write a one-page README explaining each key's type, TTL and why you chose it.

| Capstone | Redis features exercised |
| --- | --- |
| URL shortener with click analytics | Strings, INCR, HyperLogLog, TTL, sorted set of top links |
| Real-time game leaderboard API | Sorted sets, pub/sub for live updates, hashes for profiles |
| Background job system with retries | Lists or streams, consumer groups, sorted set for delayed jobs, Lua |
| Session + rate-limit gateway | Hashes, TTL, Lua sliding window, Sentinel failover |

**You are done when you can, without notes:**

- [ ] Choose the right data structure for a new problem and justify it in one sentence
- [ ] Explain RDB vs AOF and pick a durability setting for a stated loss tolerance
- [ ] Pick an eviction policy and prove it works with `INFO stats`
- [ ] Make a multi-step operation atomic with MULTI or Lua and say when each is needed
- [ ] Diagnose a slow Redis using `SLOWLOG GET`, `INFO`, `MONITOR` and `MEMORY USAGE`
- [ ] Explain what happens to your app when the primary fails

**Common mistakes to avoid:** using `KEYS *` in production, storing huge values or unbounded lists, forgetting TTLs on cache keys, and treating pub/sub as durable.
