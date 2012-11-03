package com.restphone.jartender;

public class JartenderSampleII {
	@AnnotationI(a = "i", b = @AnnotationII)
	public void testClassMethod() {
		@AnnotationI(a = "ii", b = @AnnotationII)
		Class<JartenderSampleII> x = JartenderSampleII.class;
		System.out.println(x);
	}

	@AnnotationI(a = "ii", b = @AnnotationII)
	public static String aStaticStringFieldWithAnnotation = "f";
	@AnnotationI(a = "ii", b = @AnnotationII)
	public String aFieldWithAnnotation = "ff";

	public static String aStaticStringFieldWithoutAnnotation = "fff";
	public String aFieldWithoutAnnotation = "ffff";
	
	public <T> String aGenericMethod(T x) {
		return "s";
	}
}
