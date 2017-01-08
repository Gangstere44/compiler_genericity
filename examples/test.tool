program Test {

	println(new Number().init(42));

}

class Number {

	var value : Int;
	
	def init(v : Int) : Int = {
		value = v;
		return value;
	}
}
