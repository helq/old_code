#!/usr/bin/env python2
# -*- coding: utf-8 -*-

value = { "2":2, "3":3, "4":4, "5":5, "6":6, "7":7, "8":8, "9":9, "T":10, "J":11, "Q":12, "K":13, "A":14 }

inv_value = ("", "", "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")

rank = { "Royal Flush": 10, "Straight Flush": 9, "Four of a Kind": 8, "Full House": 7, "Flush": 6, "Straight": 5, "Three of a Kind": 4, "Two Pairs": 3, "One Pair": 2, "High Card": 1 }

def sort_hand(hand):
    try:
        x = hand.pop()
    except:
        return []
    return sort_hand(filter((lambda y: value[y[0]] < value[x[0]]), hand)) + [x] + sort_hand(filter((lambda y: value[y[0]] >= value[x[0]]), hand))

is_royal_flush = lambda x: [ n + x[0][1] for n in "TJQKA" ] == x

def is_straight_flush(hand):
    low = value[hand[0][0]]
    if low < value["J"]:
        return [ inv_value[n] + hand[0][1] for n in range(low, low + 5) ] == hand, hand[-1]
    else:
        return False,

def is_four_of_a_kind(hand):
    for i in (0, 1):
        if hand[i][0] == hand[i + 3][0]:
            return True, hand[i]
    return False,

def is_full_house(hand):
    for i in (0, 2):
        if hand[i][0] == hand[i + 2][0] and hand[3 - (i/2)*3 ][0] == hand[4 - (i/2)*3][0]:
            return True, hand[i]
    return False,

def is_flush(hand):
    for i in hand[1:]:
        if hand[0][1] != i[1]:
            return False, None
    return True, hand[-1]

def is_straight(hand):
    low = value[hand[0][0]]
    if low < value["J"]:
        return [ inv_value[n] for n in range(low, low + 5) ] == [ n[0] for n in hand ], hand[-1]
    else:
        return False,

def is_three_of_a_kind(hand):
    for i in (0, 1, 2):
        if hand[i][0] == hand[i + 2][0]:
            return True, hand[i]
    return False,

def is_two_pairs(hand):
    for i in (0,1):
        if hand[i][0] == hand[i + 1][0]:
            if len(hand) == 3:
                return True, hand[i]
            else:
                final = is_two_pairs( hand[:i] + hand[i+2:] )
                if len(final) > 1:
                    if value[hand[i][0]] > value[final[1][0]]:
                        return final[0], hand[i], final[1]
                    else:
                        return final[0], final[1], hand[i]
    return False,

def is_one_pair(hand):
    for i in range(4):
        if hand[i][0] == hand[i + 1][0]:
            return True, hand[i], sort_hand(hand[:i] + hand[i+2:])
    return False, None

#hight_card = lambda x: sort_hand(x)[-1]

def ranked(hand):
    if is_royal_flush(hand):
        return rank["Royal Flush"],
    is_aux = is_straight_flush(hand)
    if is_aux[0]:
        return rank["Straight Flush"], is_aux[1]
    is_aux = is_four_of_a_kind(hand)
    if is_aux[0]:
        return rank["Four of a Kind"], is_aux[1]
    is_aux = is_full_house(hand)
    if is_aux[0]:
        return rank["Full House"], is_aux[1]
    is_aux = is_flush(hand)
    if is_aux[0]:
        return rank["Flush"], is_aux[1]
    is_aux = is_straight(hand)
    if is_aux[0]:
        return rank["Straight"], is_aux[1]
    is_aux = is_three_of_a_kind(hand)
    if is_aux[0]:
        return rank["Three of a Kind"], is_aux[1]
    is_aux = is_two_pairs(hand)
    if is_aux[0]:
        return [rank["Two Pairs"]] + list(is_aux[1:])
    is_aux = is_one_pair(hand)
    if is_aux[0]:
        aux = is_aux[2][:]
        aux.reverse()
        return [rank["One Pair"]] + [is_aux[1]] + aux
    else:
        aux = hand[:]
        aux.reverse()
        return [rank["High Card"]] + aux

def win_player1(hand1, hand2):
    player1 = ranked(hand1)
    player2 = ranked(hand2)
    if player1[0] > player2[0]:
        return True
    elif player1[0] == player2[0]:
        for i in range(1, len(player1)):
            if value[player1[i][0]] > value[player2[i][0]]:
                return True
            elif value[player1[i][0]] < value[player2[i][0]]:
                return False
    else:
        return False

hands = map( lambda x: [ [ i for i in x[n:14-n].split(" ") ] for n in (0,15) ], open("054-poker.txt", "Ur").readlines() )

for i in range(len(hands)):
    for j in (0,1):
        hands[i][j] = sort_hand(hands[i][j])

player1 = 0
player2 = 0
for i in hands:
    if win_player1(i[0], i[1]):
        player1 += 1

print player1

#hasta flush no interesa el mayor