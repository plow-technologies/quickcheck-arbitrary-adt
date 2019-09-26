# Revision history for quickcheck-arbitrary-adt

## 1.0.0.0 -- 2019-09-26

* Move code in 'Test.QuickCheck.Arbitrary.ADT' to 'Test.QuickCheck.Arbitrary.ADT.Legacy'.
* Maintain old way of producing arbitrary adts. This package should not break anything,
  unless you are importing without qualification and there is a function name clash.
* Provide a better solution without using typeclasses, 'arbitraryAdt'.
* Include tests for new functions.

## 0.3.1.0 -- 2018-01-04

* Remove compiler flags 'Wredundant-constraints' and 'fprint-potential-instances'.

## 0.3.0.0 -- 2018-01-02

* Remove unnecessary type class restrictions.
* Clean up code and import style.
* Add non Generic derived type class instances in the tests.

## 0.2.0.0 -- 2016-08-23

* Remove underscore prefix from selector names. This will break any libraries depending on 0.1.0.0, but can easily be fixed.
* Add module name to ADTArbitrarySingleton and ADTArbitrary.

## 0.1.0.0 -- 2016-08-12

* First version.
