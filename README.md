Sweet Tooth
===========

Sweet tooth locates potential application sites for syntactic abstraction.
Run sweet tooth by calling the shell script `sweet-tooth.sh`.
The script expects five arguments; the first three are:

1. The source path to some SugarJ files.
2. The path of a specific SugarJ source file relative to the source path.
3. The name of a transformation rule that is part of the SugarJ source file.

Sweet tooth will then derive a pattern for code that can be generated
by the given transformation rule.

Sweet tooth matches the derived pattern against user-provided SugarJ or Java source files.
To this end, sweet tooth expects two more arguments:

4. The source path to some SugarJ or Java files that should be matched.
5. The path of a specific SugarJ or Java source file relative to the matching path (argument 4).


Example call:

    ./sweet-tooth.sh \
      ~/projects/sugarj/case-studies/java-pet-store/src/java \
      sugar/Accessors.sugj \
      genSetters \
      ~/projects/sugarj/case-studies/java-pet-store/src/java \
      com/sun/javaee/blueprints/petstore/search/IndexDocument.sugj
