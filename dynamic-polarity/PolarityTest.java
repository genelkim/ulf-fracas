import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.util.StringUtils;
import edu.stanford.nlp.naturalli.Polarity;
import edu.stanford.nlp.naturalli.NaturalLogicAnnotations;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.Scanner;

public class PolarityTest {

 public static final StanfordCoreNLP pipeline = new StanfordCoreNLP(new Properties(){{
    setProperty("annotators", "tokenize,ssplit,pos,lemma,parse,natlog"); 
    setProperty("ssplit.isOneSentence", "true");
    setProperty("tokenize.class", "PTBTokenizer");
    setProperty("tokenize.language", "en");
    setProperty("enforceRequirements", "false");
    setProperty("natlog.neQuantifiers", "true");
  }});

  public static List<CoreLabel> annotate(String text) {
    Annotation ann = new Annotation(text);
    pipeline.annotate(ann);
    List<CoreLabel> tokens = ann.get(CoreAnnotations.SentencesAnnotation.class).get(0).get(CoreAnnotations.TokensAnnotation.class);
    return tokens;
  }

	public static void main(String[] args) {
    if (args.length == 0) {
      // Run an interactive loop if no arguments are provided.
      Scanner snr = new Scanner(System.in);
      while (true) {
        System.out.println("Write sentence for annotation ('q' to quit): ");
        String str = snr.nextLine();
  
        if (str.trim().equals("q")) {
          return;
        }
  
        List<CoreLabel> tokens = annotate(str);
        String[] tokenWords = new String[tokens.size()];
        Polarity[] polarities = new Polarity[tokens.size()];
        for (int i = 0; i < tokens.size(); ++i) {
          tokenWords[i] = tokens.get(i).word();
          polarities[i] = tokens.get(i).get(NaturalLogicAnnotations.PolarityAnnotation.class);
        }
        String wordResult = StringUtils.join(Arrays.stream(tokenWords).collect(Collectors.toList()), " ");
        String polarityResult = StringUtils.join(Arrays.stream(polarities).map(Polarity::toString).collect(Collectors.toList()), " ");
        
        System.out.println(wordResult);
        System.out.println(polarityResult);
      }
    } else if (args.length == 2) {
      // Read the file given in the first argument and write to file in the second argument. 
      try {
        System.out.println("Reading sentence from " + args[0]);
        System.out.println("Writing annotations to " + args[1]);
        BufferedReader br = new BufferedReader(new FileReader(args[0]));
        String line = br.readLine();
        List<String> lines = new ArrayList<String>();
        while (line != null && !line.trim().equals("")) {
          lines.add(line); 
          line = br.readLine();  // Assumes a single line.
        }
        br.close();

        // Annotate...
        List<String> wordResults = new ArrayList<String>();
        List<String> polarityResults = new ArrayList<String>();
        for (String curline : lines) {
          List<CoreLabel> tokens = annotate(curline);
          String[] tokenWords = new String[tokens.size()];
          Polarity[] polarities = new Polarity[tokens.size()];
          for (int i = 0; i < tokens.size(); ++i) {
            tokenWords[i] = tokens.get(i).word();
            polarities[i] = tokens.get(i).get(NaturalLogicAnnotations.PolarityAnnotation.class);
          }
          String wordResult = StringUtils.join(Arrays.stream(tokenWords).collect(Collectors.toList()), " ");
          String polarityResult = StringUtils.join(Arrays.stream(polarities).map(Polarity::toString).collect(Collectors.toList()), " ");
          wordResults.add(wordResult);
          polarityResults.add(polarityResult);
        }

        BufferedWriter bw = new BufferedWriter(new FileWriter(args[1]));
        for (int i = 0; i < wordResults.size(); i++) {
          bw.write(wordResults.get(i));
          bw.newLine();
          bw.write(polarityResults.get(i));
          bw.newLine();
        }
        bw.close();
      
      } catch (Exception e) {
        System.out.println("Error reading/writing file.");
      }
    } else {
      System.out.println("Invalid number of arguments.");
    }
  }
}

