#!/usr/bin/env python3

# Improve using spaced repetition (any of the following might work just fine):
# * https://github.com/matholroyd/tworgy-spaced-repetition
# * https://github.com/theq629/fulgurate/blob/master/cards.py
# * https://www.supermemo.com/en/archives1990-2015/english/ol/sm2
# * https://github.com/marcusbuffett/Clanki/blob/master/src/Tracker.hs

from random import choice, randint

alphabet = [
    ("Α", "α", "alpha, άλφα"),
    ("Β", "β", "beta, βήτα"),
    ("Γ", "γ", "gamma, γάμμα"),
    ("Δ", "δ", "delta, δέλτα"),
    ("Ε", "ε", "epsilon, έψιλον"),
    ("Ζ", "ζ", "zeta, ζήτα"),
    ("Η", "η", "eta, ήτα"),
    ("Θ", "θ", "theta, θήτα"),
    ("Ι", "ι", "iota, ιώτα"),
    ("Κ", "κ", "kappa, κάππα"),
    ("Λ", "λ", "lambda, λάμβδα"),
    ("Μ", "μ", "mu, μυ"),
    ("Ν", "ν", "nu, νυ"),
    ("Ξ", "ξ", "xi, ξι"),
    ("Ο", "ο", "omicron, όμικρον"),
    ("Π", "π", "pi, πι"),
    ("Ρ", "ρ", "rho, ρώ"),
    ("Σ", "σ/ς", "sigma, σίγμα"),
    ("Τ", "τ", "tau, ταυ"),
    ("Υ", "υ", "upsilon, ύψιλον"),
    ("Φ", "φ", "phi, φι"),
    ("Χ", "χ", "chi, χι"),
    ("Ψ", "ψ", "psi, ψι"),
    ("Ω", "ω", "omega, ωμέγα")]


def compare_name(char_name: str, is_lower: bool, anstrial: str) -> bool:
    comp_str = char_name.split(',')[0]
    if not is_lower:
        comp_str = 'capital ' + comp_str

    return anstrial.lower() == comp_str


if __name__ == '__main__':
    notanswered = True

    while notanswered:
        cap, lower, char_name = choice(alphabet)
        is_lower = bool(randint(0, 1))

        anstrial = input("What is the name for the greek character\n"
                         f"  {lower if is_lower else cap}: ")

        if compare_name(char_name, is_lower, anstrial):
            notanswered = False
            print("Well done! :D")
        else:
            print(f"Oops. It's name is '{'' if is_lower else 'capital '}{char_name}'. "
                  "Let's try it again\n")
