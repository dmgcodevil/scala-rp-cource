package com.dmgcodevil.rx

import rx.Observable
import rx.functions.{Action1, Func2, FuncN}


object CombineLatest {


  def first(): Observable[String] = {
    Observable.from(Array("1", "2", "3", "4"))
  }

  def second(): Observable[Int] = Observable.just(1, 2, 3, 4, 5)


  def main(args: Array[String]) {

    val combined: Observable[String] = Observable.combineLatest(first(), second(), fun())
    combined.subscribe(new Action1[String] {
      override def call(t1: String): Unit = {
        println(t1)
      }
    })
  }


  def fun() = {
    new Func2[String, Int, String]() {
      override def call(t1: String, t2: Int): String = t1 + "-" + t2
    }
  }
}
