package com.restphone.asmSample;

public class SomeSample {
	@SimpleAnnotation(a = "foo", b = @SecondAnnotation)
	public void foo() {
		Class<SomeSample> x = SomeSample.class;
		System.out.println(x);
	}
}
