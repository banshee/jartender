package com.restphone.asmSample;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

@Target(ElementType.METHOD)
public @interface SimpleAnnotation {
    @SecondAnnotation public String a();
    public SecondAnnotation b();
}
//Set(ProvidesClass[name=com/restphone/asmSample/SimpleAnnotation]
//ProvidesMethod[name=com/restphone/asmSample/SimpleAnnotation.a.()Ljava/lang/String;]
//ProvidesMethod[name=com/restphone/asmSample/SimpleAnnotation.b.()Lcom/restphone/asmSample/SecondAnnotation;])
