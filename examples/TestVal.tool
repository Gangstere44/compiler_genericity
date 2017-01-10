program TestVal {
 println(new Test().test());
}

class Number[N] {
 def mirror(a: N): N = {
  return a;
 }
}

class Double[D] extends Number[D] {
 def universe(any: D): Int = {
  return 42;
 }

 def warning(l: List[D]): List[D] = {
  return l;
 }
}

class DoubleSpe[DS] extends Number[Number[DS]] {
 def pam(any: DS): Int = {
  return 4;
 }
}

class Collection[C] {
 
}

class List[L] extends Collection[L] {
 /*
 var l: List[L];

 def init(): List[L] = {
  l = new List[L]();
  return l;
 }
 */

 def test(d: Double[L], arg: L): L = {
  return d.mirror(arg);
 }

}

class Test {

 def test(): String = {
  var d: Double[String];
  var ds: DoubleSpe[String];
  var l: List[String];
  
  var n : Number[String];
  var s : String;
  

  d = new Double[String]();
  ds = new DoubleSpe[String]();
  l = new List[String]();
  n = new Number[String]();
  
  s = n.mirror("bob");

  l = d.warning(l);
  
  n = ds.mirror(n); 

  return l.test(d, "Awesome!");
 }

}