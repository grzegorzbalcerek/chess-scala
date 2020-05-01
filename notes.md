
```
rm -f *.jar
curl -O https://repo1.maven.org/maven2/dev/zio/zio_2.13/1.0.0-RC18-2/zio_2.13-1.0.0-RC18-2.jar
curl -O https://repo1.maven.org/maven2/dev/zio/zio-stacktracer_2.13/1.0.0-RC18-2/zio-stacktracer_2.13-1.0.0-RC18-2.jar
curl -O https://repo1.maven.org/maven2/dev/zio/izumi-reflect_2.13/0.12.0-M0/izumi-reflect_2.13-0.12.0-M0.jar
curl -O https://repo1.maven.org/maven2/dev/zio/izumi-reflect-thirdparty-boopickle-shaded_2.13/0.12.0-M0/izumi-reflect-thirdparty-boopickle-shaded_2.13-0.12.0-M0.jar
export CLASSPATH='lib/*:target'
rm -f target; mkdir -p target
scalac -d target src/chess/*.scala
scala chess.App
```

https://zio.dev/docs/getting_started.html
https://javadoc.io/doc/dev.zio/zio_2.13/latest/zio/index.html
