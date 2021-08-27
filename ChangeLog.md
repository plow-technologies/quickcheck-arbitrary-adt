# Revision history for quickcheck-arbitrary-adt

## 0.3.2.0  -- 2021-08-27

* Switch to using pre-defined instance of Arbitrary in the default implementations of `toADTArbitrarySingleton` and `toADTArbitrary`

## 0.3.1.0  -- 2018-01-04

* Remove compiler flags 'Wredundant-constraints' and 'fprint-potential-instances'.

## 0.3.0.0  -- 2018-01-02

* Remove unnecessary type class restrictions.
* Clean up code and import style.
* Add non Generic derived type class instances in the tests.

## 0.2.0.0  -- 2016-08-23

* Remove underscore prefix from selector names. This will break any libraries depending on 0.1.0.0, but can easily be fixed.
* Add module name to ADTArbitrarySingleton and ADTArbitrary.

## 0.1.0.0  -- 2016-08-12

* First version.
