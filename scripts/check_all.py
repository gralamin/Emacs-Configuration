#!/usr/bin/env python
"""Check all output and format to a consistent form.

All output will match epylint.
"""
import argparse
import os
import subprocess

# comment out or add new checkers in this list.
# (program name at terminal, function to call)
ALL_PROGRAMS = [
    ("epylint", lambda: epylint),
    ("pep257", lambda: pep257),
    ("pyflakes", lambda: pyflakes),
    ("pep8", lambda: pep8),
    ("flake8", lambda: flake8)
]

VERBOSE = False


def _print_verbose(message):
    if VERBOSE:
        print message


class Line(object):

    """Sortable line that follows epylint syntax."""

    def __init__(self, text, path, line_number):
        """Generate a line object."""
        self.text = text
        self.path = path
        self.line_number = line_number

    def __cmp__(self, other):
        """Compare lines by file and line numbers."""
        try:
            ret = cmp(self.path, other.path)
            if ret == 0:
                return cmp(int(self.line_number), int(other.line_number))
            return ret
        except AttributeError:
            return super(Line, self).__cmp__(other)

    def __repr__(self):
        """Return the text of the line."""
        return self.text


class CustomLine(Line):

    """SortableLine that generates epylint syntax."""

    def __init__(self, path, line_number, error_msg, level="error", code=None,
                 location="", start_character=""):
        """Create a Line that isn't in the right syntax to begin with."""
        if code is None:
            code = level[0].upper() + "0"
        line_str = str(line_number)
        if start_character:
            line_str += ":" + start_character
        text = "%s:%s: %s (%s, %s) %s" % (
            path, line_str, level, code, location, error_msg
        )
        super(CustomLine, self).__init__(text, path, line_number)


def epylint(f):
    """Call and parse epylint."""
    popen = subprocess.Popen(["epylint", f], stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    stdout, _ = popen.communicate()
    lines = []
    output = stdout.split("\n")
    for l in output:
        if not l:
            continue
        if l.startswith("****"):
            lines.append(Line(l, "****", 0))
            continue
        if not l.strip():
            continue
        split_out = l.split(":")
        path = split_out[0]
        line_num = split_out[1]
        lines.append(Line(l, path, line_num))
    return lines


def pep257(f):
    """Call and parse pep257."""
    popen = subprocess.Popen(["pep257", f], stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    _, stderr = popen.communicate()
    output = stderr.split("\n")
    converted_output = []
    for line, next_line in zip(output[::2], output[1::2]):
        if not line:
            continue
        split_lines = line.split(":")
        path = split_lines[0]
        line_num = split_lines[1].split(" ")[0].strip()
        location = " ".join(split_lines[1].split(" ")[2:]).strip()
        split_next = next_line.split(":")
        code = split_next[0].strip()
        error_msg = split_next[1].strip()
        converted_output.append(CustomLine(path, line_num, error_msg,
                                           code=code, location=location))
    return converted_output


def pyflakes(f):
    """Call and parse pyflakes."""
    popen = subprocess.Popen(["pyflakes", f], stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    stdout, _ = popen.communicate()
    output = stdout.split("\n")
    converted_output = []
    for line in output:
        if not line:
            continue
        path_line, error = line.split(" ", 1)
        path = path_line.split(":")[0]
        line_num = path_line.split(":")[1]
        converted_output.append(CustomLine(path, line_num, error,
                                           level="warning",
                                           location="pyflakes"))
    return converted_output


def pep8(f):
    """Call and parse pep8."""
    popen = subprocess.Popen(["pep8", "--repeat", f], stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    stdout, _ = popen.communicate()
    output = stdout.split("\n")
    return _parse_pep8_style(output, "pep8")


def _parse_pep8_style(output, location):
    converted_output = []
    for line in output:
        if not line:
            continue
        path_line, code_error = line.split(" ", 1)
        path = path_line.split(":")[0]
        line_num = path_line.split(":")[1]
        start_character = path_line.split(":")[2]
        code, error = code_error.split(" ", 1)
        level = "warning"
        if code.lower().startswith("e"):
            level = "error"
        converted_output.append(CustomLine(path, line_num, error,
                                           level=level, code=code,
                                           location=location,
                                           start_character=start_character))
    return converted_output


def flake8(f):
    """Call and parse flake8."""
    popen = subprocess.Popen(["flake8", f], stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    stdout, _ = popen.communicate()
    output = stdout.split("\n")
    return _parse_pep8_style(output, "flake8")


def run_all(f):
    """Run all."""
    output = []
    programs = [program() for program_name, program in
                ALL_PROGRAMS if which(program_name)]
    for x in programs:
        new_output = x(f)
        output.extend(new_output)
    output = _filter_output(output)
    return output


def _filter_output(output):
    """Remove output that is the same from multiple sources."""
    def _filter_by_string(output, filter_strings, filter_name):
        seen_line_numbers = set()
        for line in output:
            num = int(line.line_number)
            for string in filter_strings:
                if string in line.text and num not in seen_line_numbers:
                    seen_line_numbers.add(num)
                    _print_verbose("filter %s yields possible clone '%s'" %
                                   (filter_name, line))
                    yield line
                    break
                elif string in line.text:
                    break
            else:
                _print_verbose("filter %s yields unknown line '%s'" %
                               (filter_name, line))
                yield line

    def filter_docstring_missing(output):
        docstring_missing_strings = ['Docstring missing']
        for line in _filter_by_string(output, docstring_missing_strings,
                                      "missing_docs"):
            yield line

    def filter_unused(output):
        unused_strings = ['imported but unused', 'Unused import']
        for line in _filter_by_string(output, unused_strings,
                                      "unused_import"):
            yield line

    def filter_undefined(output):
        undefined_strings = ['undefined name', 'Undefined variable']
        for line in _filter_by_string(output, undefined_strings,
                                      "undefined_var"):
            yield line

    def filter_long_lines(output):
        strings = ['line too long']
        for line in _filter_by_string(output, strings, "long_lines"):
            yield line

    # Try to sort by expected 'largest amount filtered', to 'smallest amount
    # filtered.'
    # longlines can appear anywhere, likely a good one to start with.
    # Missing docstrings are 1 per function,
    # filter_unusued is likely < 1 per function
    # Undefined is likely even smaller.
    for _filter in [
            filter_long_lines,
            filter_docstring_missing,
            filter_unused,
            filter_undefined
    ]:
        output = _filter(output)
    return list(output)


def which(program):
    """Find if a given program is installed."""
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, _ = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None


def print_output(output):
    """Output final lines to stdout."""
    output = sorted(list(set(l for l in output if l)))
    for line in output:
        print line


def is_valid_file(parser, arg):
    """Check if a file is valid before checking it."""
    if not os.path.exists(arg):
        parser.error("The file %s does not exist!" % arg)
    else:
        return arg


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(dest="filename", metavar="FILE", help="file to check",
                        type=lambda x: is_valid_file(parser, x))
    parser.add_argument("-v", "--verbose", action='store_true')
    args = parser.parse_args()
    if args.verbose:
        VERBOSE = True
    output = run_all(args.filename)
    print_output(output)
