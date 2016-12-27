
program Factorial {
    println("10! = " + new Fact().computeFactorial(10));
}


class Fact {
    def computeFactorial(num : Int) : Int = {
        var num_aux : Int;
        var arr : Int[];
     
     	arr = new Int[2];
     	println(arr.length);
        if (true || arr[3] == 1)
            num_aux = 1;
        else
           // num_aux = num * (this.computeFactorial(num - 1));
            num_aux = 42;
            
        return num_aux;
        
    }
}


