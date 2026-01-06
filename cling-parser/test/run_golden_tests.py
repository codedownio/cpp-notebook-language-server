#!/usr/bin/env python3

import os
import sys
import subprocess
import json
import difflib
from pathlib import Path

def run_test(input_file, expected_output_file):
    """Run a single golden test and compare output."""
    print(f"Running test: {input_file.name}")
    
    try:
        # Run nix run with input file
        result = subprocess.run(
            ["nix", "run"],
            input=input_file.read_text(),
            text=True,
            capture_output=True,
            check=False
        )
        
        if result.returncode != 0:
            print(f"  ❌ FAIL: Command failed with exit code {result.returncode}")
            print(f"     stderr: {result.stderr}")
            return False
            
        actual_output = result.stdout.strip()
        expected_output = expected_output_file.read_text().strip()
        
        if actual_output == expected_output:
            print(f"  ✅ PASS")
            return True
        else:
            print(f"  ❌ FAIL: Output mismatch")
            print("     Expected:")
            print(f"     {expected_output}")
            print("     Actual:")
            print(f"     {actual_output}")
            
            # Try to parse as JSON and show formatted diff if possible
            try:
                expected_json = json.loads(expected_output)
                actual_json = json.loads(actual_output)
                
                expected_formatted = json.dumps(expected_json, indent=2, sort_keys=True)
                actual_formatted = json.dumps(actual_json, indent=2, sort_keys=True)
                
                print("     Diff:")
                diff = difflib.unified_diff(
                    expected_formatted.splitlines(keepends=True),
                    actual_formatted.splitlines(keepends=True),
                    fromfile="expected",
                    tofile="actual"
                )
                print("".join(diff))
                
            except json.JSONDecodeError:
                # Fall back to line diff
                print("     Diff:")
                diff = difflib.unified_diff(
                    expected_output.splitlines(keepends=True),
                    actual_output.splitlines(keepends=True),
                    fromfile="expected", 
                    tofile="actual"
                )
                print("".join(diff))
            
            return False
            
    except Exception as e:
        print(f"  ❌ FAIL: Exception occurred: {e}")
        return False

def update_golden_files():
    """Update all golden output files with current parser output."""
    test_dir = Path(__file__).parent
    inputs_dir = test_dir / "golden" / "inputs"
    outputs_dir = test_dir / "golden" / "outputs"
    
    if not inputs_dir.exists():
        print(f"Error: Inputs directory {inputs_dir} does not exist")
        return False
        
    outputs_dir.mkdir(parents=True, exist_ok=True)
    
    for input_file in inputs_dir.glob("*.in"):
        output_file = outputs_dir / (input_file.stem + ".json")
        
        print(f"Updating {output_file.name}")
        
        try:
            result = subprocess.run(
                ["nix", "run"],
                input=input_file.read_text(),
                text=True,
                capture_output=True,
                check=True
            )
            
            output_file.write_text(result.stdout)
            print(f"  ✅ Updated")
            
        except subprocess.CalledProcessError as e:
            print(f"  ❌ Failed to update: Command failed with exit code {e.returncode}")
            print(f"     stderr: {e.stderr}")
            return False
        except Exception as e:
            print(f"  ❌ Failed to update: {e}")
            return False
    
    return True

def main():
    test_dir = Path(__file__).parent
    inputs_dir = test_dir / "golden" / "inputs"
    outputs_dir = test_dir / "golden" / "outputs"
    
    if len(sys.argv) > 1 and sys.argv[1] == "--update":
        print("Updating golden test files...")
        success = update_golden_files()
        sys.exit(0 if success else 1)
    
    if not inputs_dir.exists():
        print(f"Error: Inputs directory {inputs_dir} does not exist")
        sys.exit(1)
        
    if not outputs_dir.exists():
        print(f"Error: Outputs directory {outputs_dir} does not exist")
        print("Run with --update to create golden files")
        sys.exit(1)
    
    print("Running golden tests...")
    
    all_passed = True
    test_count = 0
    
    for input_file in sorted(inputs_dir.glob("*.in")):
        output_file = outputs_dir / (input_file.stem + ".json")
        
        if not output_file.exists():
            print(f"❌ FAIL: {input_file.name} - Missing expected output file {output_file.name}")
            all_passed = False
            continue
            
        test_count += 1
        if not run_test(input_file, output_file):
            all_passed = False
    
    print(f"\nRan {test_count} tests")
    if all_passed:
        print("✅ All tests passed!")
        sys.exit(0)
    else:
        print("❌ Some tests failed!")
        sys.exit(1)

if __name__ == "__main__":
    main()