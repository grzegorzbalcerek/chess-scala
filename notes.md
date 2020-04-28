
```
rm -f *.jar
curl -O https://repo1.maven.org/maven2/dev/zio/zio_2.13/1.0.0-RC18-2/zio_2.13-1.0.0-RC18-2.jar
curl -O https://repo1.maven.org/maven2/dev/zio/zio-stacktracer_2.13/1.0.0-RC18-2/zio-stacktracer_2.13-1.0.0-RC18-2.jar
curl -O https://repo1.maven.org/maven2/dev/zio/izumi-reflect_2.13/0.12.0-M0/izumi-reflect_2.13-0.12.0-M0.jar
curl -O https://repo1.maven.org/maven2/dev/zio/izumi-reflect-thirdparty-boopickle-shaded_2.13/0.12.0-M0/izumi-reflect-thirdparty-boopickle-shaded_2.13-0.12.0-M0.jar
export CLASSPATH=.:*
rm -f chess/*.class;
rm -f chess/*.class; scalac chess/*
rm -f chess/*.class; scalac -deprecation chess/*
scala chess.App
```
