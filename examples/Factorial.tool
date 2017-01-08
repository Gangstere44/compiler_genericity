

program Factorial {
    println(2);
    println(new Collection[Number]().setA(new Number()));//.init(3));
}

class Number {

	var value : Int;
	
	def init(v : Int) : Int = {
		value = v;
		return value;
	}
}

class Double extends Number {

}

class Collection[T] {

	var a : T;
	var b : T;
	
	def init(initA : T, initB : T) : Bool = {
	
		do(this.setA(initA));
		do(this.setB(initB));
		
		
		
		return true;
	}
	
	def p() : String = {
		return "Ok";
	}
	
	def getA() : T = {
		return a;
	}
	
	def getB() : T = {
		return b;
	}
	
	def setA(newA : T) : String = {
		a = newA;
		println(32);
		return "b43";
	}
	
	def setB(newB : T) : T = {
		b = newB;
		return b;
	}
	

}

class Test {

	var col1 : Collection[Number];
	var col2 : Collection[Number];
	
	def test(a : Int) : Int = {
		println(1);
		col1 = new Collection[Number]();
		col2 = new Collection[Number]();
		println(2);
		
		do(col1.setA(new Number()));
		do(col2.setA(new Number()));
		
		println(3);
		
		return (col1.getA().init(42) + col2.getA().init(33));
	}
		
}


class List[Z] extends Collection[Collection[Z]] {

	def initBis() : Bool = {
		
		var test : Z;
		
		do(this.setB(test));
	
		return true;
	}

}


