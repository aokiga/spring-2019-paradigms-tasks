#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "queue.h"
#include "doctest.h"

TEST_CASE("Queue inited and destroyed") {
    Queue q;
    queue_init(&q);
    CHECK(queue_empty(&q));
    queue_destroy(&q);
}

TEST_CASE("Queue pushes and pops") {
    Queue q;
    queue_init(&q);

    int a = 0, b = 0, c = 0;
    queue_push(&q, &a);
    CHECK(!queue_empty(&q));
    queue_push(&q, &b);
    CHECK(!queue_empty(&q));
    queue_push(&q, &c);
    CHECK(!queue_empty(&q));

    CHECK(queue_pop(&q) == &a);
    CHECK(!queue_empty(&q));
    CHECK(queue_pop(&q) == &b);
    CHECK(!queue_empty(&q));
    CHECK(queue_pop(&q) == &c);
    CHECK(queue_empty(&q));

    queue_destroy(&q);
}

TEST_CASE("Queue can be refilled") {
    Queue q;
    queue_init(&q);

    int a = 0, b = 0, c = 0;
    queue_push(&q, &a);
    queue_push(&q, &b);
    CHECK(queue_pop(&q) == &a);
    CHECK(queue_pop(&q) == &b);
    CHECK(queue_empty(&q));

    queue_push(&q, &c);
    CHECK(!queue_empty(&q));
    queue_push(&q, &b);
    CHECK(!queue_empty(&q));
    CHECK(queue_pop(&q) == &c);
    CHECK(!queue_empty(&q));
    CHECK(queue_pop(&q) == &b);
    CHECK(queue_empty(&q));

    queue_destroy(&q);
}
