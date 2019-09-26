def zoeken(element, lijst):
    begin = 0
    einde = len(lijst) - 1
    while begin <= einde:
        m = (begin + einde) // 2
        if lijst[m] == element:
            return m
        elif lijst[m] > element:
            einde = m - 1
        else:
            begin = m + 1
    return -1
