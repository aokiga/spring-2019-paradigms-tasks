#include "queue.h"
#include <assert.h>
#include <stdlib.h>

/**
 * Очередь реализована как односвязный список.
 * Начало списка соответствует концу очереди, т.е. элементы добавляются в
 * начало, а удаляются из конца.
 */
struct QueueNode {
    void *data;
    QueueNode *next;
};

void queue_init(Queue *q) {
    q->head = nullptr;
    q->last = nullptr;
}

void queue_destroy(Queue *q) {
    assert(!q->head);
}

bool queue_empty(Queue *q) {
    return !q->head;
}

void queue_push(Queue *q, void *data) {
    QueueNode *node = static_cast<QueueNode *>(malloc(sizeof(QueueNode)));
    node->data = data;
    node->next = nullptr;
    if (q->last) {
        q->last->next = node;
    } else {
        q->head = node;
    }
    q->last = node;
}

void *queue_pop(Queue *q) {
    QueueNode *node = q->head;
    assert(node);
    void *data = node->data;
    q->head = node->next;
    if (!q->head) {
        q->last = nullptr;
    }
    free(node);
    return data;
}
