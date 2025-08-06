# Grammar rules

```math
\begin{align}

\text{pattern} &\to \text{alternation} \\

\text{alternation} &\to \text{concatenation (| concatenation)*} \\

\text{concatenation} &\to \text{quantified\_atom}+ \\

\text{quantified\_atom} &\to \text{atom} \space \text{quantifier}? \\

\text{atom} &\to \begin{cases}
    literal \\
    \text{group} \\
    \text{class} \\
    \text{.}
\end{cases} \\

\text{quantifier} &\to \begin{cases}
    * \\
    + \\
    ? \\
    \{ digit(,digit?)? \}
\end{cases} \\

\text{group} &\to \text{(pattern)} \\

\text{class} &\to [(literal| \text{range})+] \\

\text{range} &\to literal-literal

\end{align}
```
