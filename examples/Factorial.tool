
program Factorial {
    println("10! = " + new Fact[Fact[String]]().computeFactorial(10));
}


class Fact[T] {

	var b : Int;
	
	var nn : Node[Node[String]];
	var gg : Node[String]; 
	var bb : Node[T]; 
	var bn : T;

    def computeFactorial(num : Int) : Int = {
        var num_aux : Int;
                
        if (true)
            num_aux = 1;
        else
           // num_aux = num * (this.computeFactorial(num - 1));
            num_aux = 42;
            
        return num_aux;
        
    }
}

class Node[T] {

	var a : T;
}


