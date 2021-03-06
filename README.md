## Generically Generating AI

Uses genetic programming to create an intelligent agent for arbitrary adversarial circumstances. Currently, this uses tic-tac-toe as a case study.

There's a small core of situation-specific AI code which forms the gene pool. These are all incredibly simplistic ("if the corners are filled, consider filling the center", or "if there are two in a row, consider filling the third cell in the series"). The main AI engine, though, combines these genes to form a new agent. Genes deemed helpful are replicated and thus given more importance. Genes deemed unhelpful are sifted out from one generation to the next. (Incidentally, this makes the process of gene creation more painless--there is no major penalty with creating truly unhelpful genes.) Along the way, some genes are randomly mutated to increase diversity--otherwise performance would be constrained by the set defined by all linear combinations of the gene pool, which might not have a worthwile solution.

To develop a good agent, many individuals are created and paired off, and each pair plays against each other. The winners then mate (combining genes), and the resulting individuals repeat the process until only one remains.

This program received an award from the IEEE and another from the US Air Force at the 2011 Pittsburgh Regional Science and Engineering Fair.
