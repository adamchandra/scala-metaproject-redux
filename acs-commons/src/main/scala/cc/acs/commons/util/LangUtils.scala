package cc.acs.commons.util

object LangUtils {
	/**
	 * Given a list of maps of key->[vals], concatenate the value lists
	 * for each key.
	 * Example: dictMerge( {1: [a, b], 2: [d, e]}, {1: [f, g], 3: [h]} ) =>
	 *     {1: [a, b, f, g], 2: [d, e], 3: [h]}
	 */
  // type Multimap[A, B] = MapLike[A, Seq[B], Map[A, Seq[B]]]
  // def dictMerge[A, B](m1:Multimap[A, B], m2:Multimap[A, B]): Multimap[A, B] = {

  def dictMerge[A, B](m1:Map[A, List[B]], 
                      m2:Map[A, List[B]]): Map[A, List[B]] = {
    m1.map {case (i, l) => (i, l ++ m2.getOrElse(i, Nil))} ++ 
    m2.map {case (i, l) => (i, m1.getOrElse(i, Nil) ++ l)}}

//   def dictMerge[A, B](m1:MapLike[A, Seq[B], Map[A, Seq[B]]], 
//                       m2:MapLike[A, Seq[B], Map[A, Seq[B]]]): MapLike[A, Seq[B], Map[A, Seq[B]]] = {
//     m1.map {case (i, l) => (i, l ++ m2.getOrElse(i, Nil))} ++ 
//     m2.map {case (i, l) => (i, m1.getOrElse(i, Nil) ++ l)}}
// 

	/**
	 * Turns a list of maps into a map of maps. Takes the specified key
	 * from each map, and uses its value as the key for that map in the
	 * returned map. Kinda like inverted indexing.
	 */
	// public static Map invertMapsOnKey(List<Map> list, Object k) {
	// return ldict( 				interleave(map(dictGet(k), list), list));  
	

	/**
	 * generate a random integer between [0, n)
	 * @param n
	 * @return
	 */
	// public static int random(int n) { return (int)Math.floor(n*Math.random()); }
  
	// public static Object randomElement(Collection c) {
	// 	int x = random(c.size());
	// 	Iterator i = c.iterator();
	// 	while (x-- > 0 && i.hasNext()) {
	// 		i.next();
	// 	}
	// 	return i.next();
	// }
	// 
	// public static Object choose1(Collection c) {
	// 	return chooseN(1, c).get(0);
	// }
	
	/**
	 * <pre>
	 * Choose n elements from collection c.
	 * Returns the elements in same order they appeared in original collection. 
	 * </pre>
	 */
	// public static List chooseN(int n, Collection c) {
	// 	List<Integer> range = range(c.size());
	// 	List<Integer> indices =  list();
	// 	for (int i = 0; i < n; i++) {
	// 		Integer elem = cast(randomElement(range));
	// 		range.remove(elem);
	//     indices.add(elem);
  //   }
	// 	sortInPlace(indices);
	// 	List l = list();
	// 	for (int i : indices) {
	// 		l.add(CollectionUtils.get(c, i));
  //   }
	// 	return l;
	// }
	// 
	// public static List<String> prepend(String s, List<String> strings) {
	//   return map(prefix(s), strings);
  // }
  // 
	// public static List<String> indent(int i, List<String> strings) {
	// 	String whitespace = join("", listof(i, " "));
	//   return prepend(whitespace, strings);
  // }
	// 
	// public static List<String> lines(String strings) {
	// 	return aslist(strings.split("\n"));
  // }
  // 
  // /**
  //  * Given two collections, compute ratio for how well they match.
  //  * 
  //  * @param c1
  //  * @param c2
  //  * @return  Between 0.0 (no members in common) and 1.0 (identical)
  //  */
  // public static double computeSimilarity(Collection c1, Collection c2) {
  //   Set union = toSet(union(c1, c2));
  //   if (union.isEmpty()) { return 1.0; } // Two empty lists are identical.
  //   Set intersection = toSet(intersection(c1, c2));
  // 
  //   double ratio = (double)intersection.size() / (double)union.size();
  //   return ratio;
  // }
  // 
  // public static Set createEnglishDictionary() {
  // 	return createEnglishDictionary(LangUtils.class.getResource("words.txt"));
  // }
  // 
  // public static Set createEnglishDictionary(URL wordlist) {
  // 	return set(FileUtils.lines(file(wordlist.getFile())));
  // }
  // public static Set createEnglishDictionary(File wordlist) {
  // 	return set(FileUtils.lines(wordlist));
  // }
}
