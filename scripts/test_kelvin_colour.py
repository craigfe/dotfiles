import pytest
import unittest
import kelvin_colour as kc

class ScriptTests(unittest.TestCase):

    def test_emptyParams(self):
       assert (kc.main(['test_kelvin_colour.py']) == 1)

    def test_nonInteger(self):
        assert (kc.main(['test_kelvin_colour.py', '0K']) == 2)

    def test_clamp(self):
        assert kc.clamp(0) == 0
        assert kc.clamp(255) == 255
        assert kc.clamp(123) == 123
        assert kc.clamp(3.14159265) == 3
        assert kc.clamp(254.5001) == 255
