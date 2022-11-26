id:: 63825d4d-86bd-4a94-b55a-dff15509f864
$$F:\mathcal{L}\vdash\mathcal{P}\rightarrow\mathcal{L'}\vdash\mathcal{P}$$

- Compiling to OpenQASM 3.0
	- #+BEGIN_NOTE
	  It is expected that compilers for OpenQASM 3 will support all of the classical operations specified in this document for values that can be reasonably inferred to be compile-time constants, and will perform these operations at compile time. 
	  #+END_NOTE
	  At a minimum, “reasonably inferred” means values declared const, and literals.
	  
	  “scientific-calculator functions” such as sin, exp, and so on will always work on expressions involving only literals and values declared const of compatible types, and the compiler will completely fold such expressions into a single constant.
	  
	  An implementation of OpenQASM 3 is permitted to reject programs that use [types] if the hardware has no facilities to support them. Similarly, even if a hardware implementation accepts values declared complex[float[64]], it is not required to accept programs that use the infix ** power operator on them at runtime, but a compiler is required to evaluate such operator expressions if the operands are compile-time known.
	  
	  Hardware implementations that support a particular feature must follow the rules for it given in this specification, unless such a feature is specifically stated to be “implementation-defined”. If they cannot, then they must not accept programs that use that feature. The user can therefore expect that if an OpenQASM 3 program accepted by two implementations, both will perform the same behaviour except in cases this document explicitly allows it to differ.
- [[OpenQASM File Spec]]
- [[OpenQASM Language Spec]]