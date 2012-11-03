package com.restphone.jartender;

public class JartenderSample implements InterfaceI {
	class JartenderSampleSubclass {
		
	}
	
	@AnnotationI(a = "i", b = @AnnotationII)
	public void testClassMethod() {
		@AnnotationI(a = "ii", b = @AnnotationII)
		Class<JartenderSample> x = JartenderSample.class;
		System.out.println(x);
		
		// Use a static field
		System.out.println(JartenderSampleII.aStaticStringFieldWithAnnotation);
		
		JartenderSampleII j = new JartenderSampleII();
		System.out.println(j.aFieldWithAnnotation + j.aGenericMethod("j"));
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
