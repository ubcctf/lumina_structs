from setuptools import setup

setup(name='lumina_structs',
      version='0.1',
      description='Protocol definitions for Lumina',
      author='Synacktiv',
      maintainer='Maple Bacon',
      packages=['lumina_structs'],
      package_dir={
        'lumina_structs': '.',
      },
      include_package_data=True,
      exclude_package_data={
        '': ['setup.py']
      })