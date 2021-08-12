module CFG (
    CFG (CFG),
    variables, transitions
) where


data Character var = Terminal Sigma | Variable var

data CFG var = CFG {
    variables :: var,
    transitions :: Map var (Character var),
    start :: var
}


