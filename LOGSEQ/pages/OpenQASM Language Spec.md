- Identifiers must begin with a letter [A-Za-z], an underscore or an element from the Unicode character categories Lu/Ll/Lt/Lm/Lo/Nl
  #+BEGIN_NOTE
   Identifiers may not override a reserved identifier.
  #+END_NOTE
- Types
	- bool
	- int
	- uint
	- float
	- angle
	- bit
	- duration
	- qubit
- Gate
	- ```
	  gate name(params) qargs) 
	  {
	      body
	  }
	  ```
	- Modifiers
	  $$m:U\in\mathcal{H}\rightarrow U'\in\mathcal{H}'\subseteq\mathcal{H}$$
		- `ctrl @`
			- The modifier ctrl @ replaces its gate argument U by a controlled-U gate. If the control bit is 0, nothing happens to the target bit. If the control bit is 1, U acts on the target bit. Mathematically, the controlled-U gate is defined as CU=I⊗Uc, where c is the integer value of the control bit and CU is the controlled-U gate. The new quantum argument is prepended to the argument list for the controlled-U gate. The quantum argument can be a register, and in this case controlled gate broadcast over it (as it shown in examples above for CNOT gate). The modified gate does not use any additional scratch space and may require compilation to be executed.
- Directives
	- Pragma
	- Annotations
	- input
	- output