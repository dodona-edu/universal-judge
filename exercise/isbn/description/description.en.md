In the ISBN-10 (_International Standard Book Numbering_) system that was used until the end of 2006, each book is assigned a unique 10-digit code. The first nine digits uniquely identify the book itself, whereas the last digit merely serves as a check digit to detect invalid ISBN-10 codes.

<div class="dodona-centered-group">
<img src="media/ISBN.gif" alt="IBSN">
</div>

If $$x_1, \ldots, x_9$$ represent the first nine digits of an ISBN-10 code, the check digit $$x_{10}$$ is calculated as

$$
 x_{10} = (x_1+ 2x_2+ 3x_3+ 4x_4+ 5x_5+ 6x_6+ 7x_7+ 8x_8+ 9x_9)\!\!\!\!\mod{11}
$$
 
 As a result, the check digit $$x_{10}$$ always takes a value in between 0 and 10\. If the check digit is equal to 10, it is represented by the uppercase letter X in the ISBN-10 code. As such, only a single character is needed to represent the check digit.

In the new ISBN-13 system, each book is assigned a unique 13-digit code. The first twelve digits identify the book itself, whereas the last digit merely serves as a check digit to detect invalid ISBN-13 codes. If $$x_1, \ldots, x_{12}$$ represent the first twelve digits of an ISBN-13 code, the check digit $$x_{13}$$ is calculated as
 
 $$
 \begin{align}
  o &= x_1 + x_3 + x_5 + x_7 + x_9 + x_{11} \\
  e &= x_2 + x_4 + x_6 + x_8 + x_{10} + x_{12} \\
  x_{13} &= (10 - (o + 3e) \mod{10}) \mod{10}
 \end{align}
 $$
 
 As a result, $$x_{13}$$ always takes a value in between 0 and 9, so that ISBN-13 codes only contain digits.

### Assignment

*   Write a function `is_isbn` that takes a string $$c$$ (`str`). The function must return a Boolean value (`bool`) that indicates whether $$c$$ is a valid ISBN code. The function also has an optional second parameter `isbn13` that may take a Boolean value (`bool`) indicating whether the function must check for an ISBN-10 code (`False`) or for an ISBN-13 code (`True`, default value).

*   Write a function `are_isbn` that takes a `list` containing $$n \in \mathbb{N}$$ codes. The function must check whether each code in the given list is a valid ISBN code. The function also has an optional second parameter `isbn13` that may take a Boolean value (`bool`) indicating whether the function must check for ISBN-10 codes (`False`) or for ISBN-13 codes (`True`).

    If no value is explicitly passed to the parameter `isbn13`, the type of the code must be derived from its length. Codes that are no strings (`str`) are considered to be invalid by definition. Codes having length 13 must be checked as ISBN-13 codes and codes having length 10 must be checked as ISBN-10 codes. Codes having a length that deviates from 10 and 13 are considered to be invalid by definition.

    The function must return a new `list` containing $$n$$ Boolean values (`bool`) that indicate whether the code at the corresponding position in the given list is a valid ISBN code.

### Example

The following examples are in Python.

```pydocstring
>>> is_isbn('9789027439642', False)
False
>>> is_isbn('9789027439642', True)
True
>>> is_isbn('9789027439642')
True
>>> is_isbn('080442957X')
False
>>> is_isbn('080442957X', False)
True

>>> codes = ['0012345678', '0012345679', '9971502100', '080442957X', 5, True, 'The Practice of Computing Using Python', '9789027439642', '5486948320146']
>>> are_isbn(codes)
[False, True, True, True, False, False, False, True, False]
>>> are_isbn(codes, True)
[False, False, False, False, False, False, False, True, False]
>>> are_isbn(codes, False)
[False, True, True, True, False, False, False, False, False]
```

### Ask Pythia …

In the following instruction video, Pythia explains how to tackle this convert_statement. Watch this video as a stepping stone to solve other exercises about [advanced functions and modules](https://dodona.ugent.be/en/exercises/?filter=opgaven/reeks07).

<div class="dodona-centered-group"><iframe src="https://www.youtube.com/embed/BVchG1WSJk4" allow="autoplay; encrypted-media" allowfullscreen="" height="315" width="560"></iframe></div>
