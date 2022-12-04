import unittest
import pathlib
from Cython.Build.Cythonize import main as cythonize


DIR_PATH = pathlib.Path(__file__).parents[2] / "tests/samples/cython"


class TestCompile(unittest.TestCase):
    filename = ""
    extension = ".pyx"

    def setUp(self):
        assert self.filename, "Filename not specified"
        assert (DIR_PATH / self.filename).is_file(), "Filepath for file: {} not found".format(self.filename)

    def tearDown(self):
        exts = ["c", "cpp", "so"]
        for ext in exts:
            for filename in DIR_PATH.glob("*.{}".format(ext)):
                filename.unlink()

    def _run_test(self):
        cythonize([self.filepath, "-i", "-b"])

    @property
    def filename(self) -> str:
        if not self._testMethodName:
            return ""
        return self._testMethodName.replace("test_", "") + self.extension

    @property
    def filepath(self) -> str:
        return str(DIR_PATH / self.filename) if self.filename else ""


if __name__ == '__main__':
    if not DIR_PATH.is_dir():
        raise NotADirectoryError("Directory: {} not found".format(str(DIR_PATH)))
    runner = unittest.TextTestRunner()
    suite = unittest.TestSuite()
    for filepath in DIR_PATH.iterdir():
        if not filepath.is_file() or not filepath.suffix == TestCompile.extension:
            continue
        test_name = "test_" + filepath.stem
        setattr(TestCompile, test_name, TestCompile._run_test)
        suite.addTest(TestCompile(test_name))
    runner.run(suite)
