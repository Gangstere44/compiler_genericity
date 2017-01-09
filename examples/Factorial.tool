

program Factorial {
    println(2);
    println(new Test().test(1));
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
	
	def setA(newA : T) : T = {
		a = newA;
		println(32);
		return a;
	}
	
	def setB(newB : T) : T = {
		b = newB;
		return b;
	}
	
	
	def tt(newC: T, newD: Int, newE: Bool) : Bool = {
	
		return true;
	
	}
	

}

class A {

}

class Test {

	var col1 : Collection[Number];
	var col2 : Collection[Number];
	
	var l1 : List[String];
	var l2 : List[String];
	var l3 : List[Collection[Number]];
	var l4 : List[Collection[Double]];
	var l5 : List[List[Double]];
	
	def test(a : Int) : Int = {
		
		println("test1");
		println(1);
		col1 = new Collection[Number]();
		col2 = new Collection[Number]();
		println(2);
		
		do(col1.tt(new Number(), 42, true));
		
		do(col1.init(new Number(), new Double()));
		
		do(col1.setA(new Number()));
		do(col2.setA(new Number()));
		
		println(3);
		
		println(col1.getA().init(42) + col2.getA().init(33));
		
		
		//l1 = new List[String]();
		l2 = new List[String]();
		l3 = new List[Collection[Number]]();
		l4 = new List[Collection[Double]]();
		l5 = new List[List[Double]]();
		
		l1 = new List[String]();
		
		println("test 2");
		
		do(l1 == l2);
		do(l3 == l4);
		do(l3 == l5);
		
		do(l1.initBis(new Collection[String]()));
		
		println(l2.initCreation("ok + ") + l1.initCreation(" _ 3 "));
		
		return 43;
	}
		
}


class List[Z] extends Collection[Collection[Z]] {

	var text : Z;

	def initBis(in : Collection[Z]) : Bool = {		
		
		do(this.setB(in));
	
		return true;
	}
	
	def initCreation(t : Z) : Z = {
		
		text = t;
		
		return t;
		
	}

}


