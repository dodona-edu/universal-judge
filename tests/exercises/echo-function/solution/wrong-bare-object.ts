function echo(content: Object): Object {
    const answer = Object.create(null) as Record<string, number>;
    answer.a = 1;
    return answer;
}
