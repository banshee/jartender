package com.restphone.asmSample;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

@Target(ElementType.METHOD)
public @interface SimpleAnnotation {
    @SecondAnnotation public String a();
    public SecondAnnotation b();
}
