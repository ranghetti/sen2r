from setuptools import setup

setup(name='FeLS',
      version='1.2.9',
      description='Fetch Landsat & Sentinel Data from google cloud',
      url='https://github.com/vascobnunes/fetchLandsatSentinelFromGoogleCloud',
      author='vascobnunes',
      author_email='vascobnunes@gmail.com',
      license='GPL',
      zip_safe=False,
      install_requires=['numpy'],
      dependency_links=['https://www.conan.io/source/Gdal/2.1.3/osechet/stable'])