import pytest
import unittest

import kelvin_colour

class ScriptTests(unittest.TestCase):
    def test_main(self):
        with self.assertRaises(SystemExit):
            kelvin_colour.main(['test_kelvin_colour.py']) # Empty
