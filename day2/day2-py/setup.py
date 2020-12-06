from distutils.core import setup

setup(
    name="day2-py",
    version="1.0.0",
    packages=["password"],
    entry_points="""
        [console_scripts]
        day2-py=password.__main__:main
    """,
)
