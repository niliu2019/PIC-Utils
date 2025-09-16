# PIC-Utils
A Utility Module Collection for PIC/MCC Solver

## Module Overview

| Module Name                  | File Name                   | Brief Description                                                                 |
|------------------------------|-----------------------------|-----------------------------------------------------------------------------------|
| Differential Interpolation   | UtilDiffInterpolation.f90   | Provides various numerical differentiation and interpolation methods               |
| Timer                        | UtilTimer.f90               | Implements a timer system with value transition and callback support               |
| Dynamic Vector               | UtilVector.f90              | Implements a dynamic, resizable array for Real(8) values with memory management    |
| Register Dump               | UtilRegisterDump.f90        | Efficient variable registration and data output system for simulation diagnostics  |

---

## 1. Differential Interpolation Module
**File:** `UtilDiffInterpolation.f90`

This module offers a comprehensive set of numerical differentiation formulas, including forward, backward, and central differences from first to fifth order. It also provides named interpolation schemes such as Lagrange, Hermite, Spline, Adams-Bashforth, and Adams-Moulton methods. These tools are essential for accurate numerical analysis and solving differential equations in scientific computing.

**Features:**
- Standard finite difference formulas (1st to 5th order)
- Named interpolation methods for advanced numerical analysis
- Flexible interfaces for time series and array-based data

**Use Cases:**
- Numerical differentiation of simulation data
- Implementing custom ODE solvers
- High-accuracy interpolation in scientific models

---

## 2. Timer Module
**File:** `UtilTimer.f90`

This module provides a robust timer system for managing simulation time and cycles. It supports automatic value transitions over specified cycle ranges and allows users to register callback functions for custom actions during transitions. The timer is designed for use in iterative simulations where precise control over time and value changes is required.

**Features:**
- Tracks simulation time and cycles
- Supports automatic value transitions with linear interpolation
- Callback interface for custom transition actions
- Easy initialization and update routines

**Use Cases:**
- Managing simulation steps and time progression
- Automating parameter changes during simulation
- Triggering events or actions at specific simulation cycles

---

## 3. Dynamic Vector Module
**File:** `UtilVector.f90`

This module implements a dynamic vector (resizable array) data structure for Real(8) values. It provides efficient memory management, automatic resizing, and safe access methods. The vector can grow or shrink as elements are added or removed, making it ideal for applications requiring flexible data storage.

**Features:**
- Automatic memory management with grow/shrink capabilities
- Bounds checking for safe array access
- Efficient memory reallocation strategy (doubling/halving)
- Memory leak prevention with proper deallocation

**Use Cases:**
- Storing and manipulating simulation data
- Implementing dynamic lists or buffers
- Managing large datasets with variable size

---

## 4. Register Dump Module
**File:** `UtilRegisterDump.f90`

This module provides an efficient system for registering, tracking, and outputting simulation variables to files. It features optimized variable lookup, automatic file management, and formatted data output capabilities, making it ideal for simulation diagnostics and result analysis.

**Features:**
- Fast variable registration with hash-based lookup
- Efficient memory management with dynamic resizing
- Automatic header generation and formatted output
- Pointer-based variable tracking for zero-copy updates
- Support for various file opening modes and output formats

**Use Cases:**
- Simulation diagnostics and data collection
- Generating time series output for post-processing
- Performance monitoring and analysis
- Creating formatted data files for scientific visualization

---
