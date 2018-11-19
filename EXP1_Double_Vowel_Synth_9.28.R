##################################################################
#
#
# Synthesizing stimuli : Single and double vowels
#
# Michelle Cohn, last updated 9.28.2016
##################################################################

# Open phonTools app
library(phonTools)

library(Rmpfr) # Open Rmpfr package to preallocate array used to index vowels (and later combine)
library(PraatR)


#########################################
#
# Set version (for counterbalancing the buttons)
# version = "B"


########################### Loop through all counterbalanced orders A:E ##########################
versionIDs = c("A", "B", "C", "D", "E")
nversions = length(versionIDs)

for(v in 1:nversions){
  version = versionIDs[v]
  
  ####################################
  
  
  # Define directories 
  singlevdir = sprintf("/Users/michellecohn/Desktop/Stimuli/Version%s/Single_V", version)
  dbsinglevdir = sprintf("/Users/michellecohn/Desktop/Stimuli/Version%s/DB_Adjusted/Single_Vowels", version)
  doublevdir = sprintf("/Users/michellecohn/Desktop/Stimuli/Version%s/Double_V", version)
  doublevoutput = sprintf("/Users/michellecohn/Desktop/Stimuli/Version%s/DB_Adjusted/Double_Vowels", version)
  
  
  ######################
  #
  # E-prime info
  
  #Directory for stimulus lists 
  eprime_stim_dir = sprintf("/Users/michellecohn/Desktop/Stimuli/Version%s/Eprime_Stim_List", version)
  
  # Procedure name for single vowels
  singlevowelprocedure = "trialproc"
  
  # Procedure name for double vowels
  doublevowelprocedure = "vowelproc"
  
  
  #
  # Write a function to specify the path (for later use in Praat R)
  
  
  SingleVPath = function(FileName){ return( paste( sprintf("%s/", singlevdir), FileName, sep="") ) }
  DBSingleVPath = function(FileName){ return( paste( sprintf("%s/", dbsinglevdir), FileName, sep="") ) }
  DoubleVPath = function(FileName){ return( paste( sprintf("%s/", doublevdir), FileName, sep="") ) }
  DBDoubleVPath = function(FileName){ return( paste( sprintf("%s/", doublevoutput), FileName, sep="") ) }
  
  
  
  #
  # Define spectrogram directory
  spectrogram_dir = sprintf("/Users/michellecohn/Desktop/Stimuli/Version%s/Spectrograms", version)
  
  ###################
  # 
  # Specify target dB for output .wav
  
  dB = 70
  
  #####################
  #
  # 1. Define vowel variables
  #
  ####################
  
  # Define new FF 
  vowel1 = "i"
  # vowel1FF = c(391, 2200, 2500, 3500, 4500) # Holland (2014) 
  # vowel1FF = c(390, 2200, 2500, 3500, 4500) # Rounded
  vowel1label = "I"
  
  vowel1FF = c(350, 2400, 2500, 3500, 4500)  # Adjusted <<<
  #vowel1FF = c(350, 2400, 3050, 3500, 4500)  # F3 based on Summers & Leek (1998)
  vowel1NUMID = 100
  
  
  vowel2 = "ɛ"
  # vowel2FF = c(701, 1855, 2500, 3500, 4500) # sounds more like /æ/
  vowel2FF = c(550, 1850, 2500, 3500, 4500) 
  vowel2NUMID = 200
  vowel2label = "EH"
  
  
  vowel3 = "æ"
  # vowel3FF = c(853, 1784, 2500, 3500, 4500) # Based on Holland (2014) averages
  vowel3FF = c(800, 1780, 2500, 3500, 4500)
  vowel3NUMID = 300
  vowel3label = "AE"
  
  
  vowel4 = "ɑ"
  # vowel5FF = c(832, 1380, 2500, 3500, 4500) 
  vowel4FF = c(850, 1380, 2500, 3500, 4500) 
  vowel4NUMID = 400
  vowel4label = "AH"
  
  
  vowel5 = "u"
  # vowel5FF = c(400, 1600, 2500, 3500, 4500) # fronted goose
  
  vowel5FF = c(400, 1600, 2250, 3500, 4500) 
  # vowel5FF = c(250, 850, 2250, 3500, 4500) # F3 based on Summers & Leek (1998)
  vowel5NUMID = 500
  vowel5label = "U"
  
  
  # Combine into a single matrix to index later
  # FFmatrix = rbind(vowel1FF, vowel2FF, vowel3FF, vowel4FF, vowel5FF)
  #vowelNUMID = c(100, 200, 300, 400, 500)
  

  
  if(version == "A"){
    FFmatrix = rbind(vowel1FF, vowel2FF, vowel3FF, vowel4FF, vowel5FF)
    vowelNUMID = c(100, 200, 300, 400, 500)
    vowellabels = c(vowel1label, vowel2label, vowel3label, vowel4label, vowel5label)
  }
  
  if(version == "B"){
    FFmatrix = rbind(vowel2FF, vowel3FF, vowel4FF, vowel5FF, vowel1FF)
    vowelNUMID = c(200, 300, 400, 500, 100)
    vowellabels = c(vowel2label, vowel3label, vowel4label, vowel5label, vowel1label)
    
  }
  
  if(version == "C"){
    FFmatrix = rbind(vowel3FF, vowel4FF, vowel5FF, vowel1FF, vowel2FF)
    vowelNUMID = c(300, 400, 500, 100, 200)
    vowellabels = c(vowel3label, vowel4label, vowel5label, vowel1label, vowel2label)
    
  }
  
  
  if(version == "D"){
    FFmatrix = rbind(vowel4FF, vowel5FF, vowel1FF, vowel2FF, vowel3FF)
    vowelNUMID = c(400, 500, 100, 200, 300)
    vowellabels = c(vowel4label, vowel5label, vowel1label, vowel2label, vowel3label)
    
  }
  
  if(version == "E"){
    
    FFmatrix = rbind(vowel5FF, vowel1FF, vowel2FF, vowel3FF, vowel4FF)
    vowelNUMID = c(500, 100, 200, 300, 400)
    vowellabels = c(vowel5label, vowel1label, vowel2label, vowel3label, vowel4label)
    
  }
  
  
  
  # FFmatrixVersionA = rbind(vowel1FF, vowel2FF, vowel3FF, vowel4FF, vowel5FF)
  # FFmatrixVersionB = rbind(vowel2FF, vowel3FF, vowel4FF, vowel5FF, vowel1FF)
  # FFmatrixVersionC = rbind(vowel3FF, vowel4FF, vowel5FF, vowel1FF, vowel2FF)
  # FFmatrixVersionD = rbind(vowel4FF, vowel5FF, vowel1FF, vowel2FF, vowel3FF)
  # FFmatrixVersionE = rbind(vowel5FF, vowel1FF, vowel2FF, vowel3FF, vowel4FF)
  # 
  
  
  
  ####################
  
  # Total number of vowels
  vowelIDs = c(vowel1, vowel2, vowel3, vowel4, vowel5) # For later access in a loop
  nvowels = length(vowelIDs)
  
  # Plot vowels in vowel space (F1, F2)
  f1s = as.vector(FFmatrix[,1])
  f2s = as.vector(FFmatrix[,2])
  
  vowelplot(f1s, f2s, labels = vowelIDs, alternateAxes = TRUE, xsampa = FALSE, xrange = c(250, 1000), yrange = c(500, 3000), main = "Formant Frequency Values in Hertz (Hz)")
  
  # vowelplot(f1s, f2s, labels = vowelIDs, alternateAxes = TRUE, xsampa = FALSE, xrange = c(300, 1000), yrange = c(1000, 2500), main = "Formant Frequency Values in Hertz (Hz)")
  
  duration = 260 
  samplingrate = 10000
  
  
  ######################
  #
  # 2. Define pitch levels
  #
  # Fundamental frequency (concatenate if starting / ending values differ)
  # f0 = c(120)
  ######################
  
  origf0 = 120
  stlevels = c(0, 0.156, 0.306, 1, 2, 3)
  npitch = length(stlevels)
  
  # define the semitone difference for descending contour
  startf0 = 120
  #endf0 = 115
  # descendST = 12* log10 (endf0/startf0)/log10(2)
  
  #############################################################################################
  #############################################################################################
  
  #############################
  # Loop through pitch levels, saving them as variables
  # Saving them in a matrix (to access later)
  
  
  pitchmatrix = zeros(npitch, 2) # initialize a matrix to store the values
  
  for(p in 1:npitch){
    halfsteps = 2^(1/12)
    startf0 = origf0[1] * ((halfsteps)^(stlevels[p]))
    # endf0 = startf0 * ((halfsteps))^(descendST)
    
    pitchmatrix[p,1] = startf0[1]
   #  pitchmatrix[p,2] = endf0[1]
    
    
  } #pitch loop
  
  
  
  #####################
  # 
  # 3. Create a textfile load in all vowel stim names (.wav) and attributes for E-prime StimList
  #
  #####################
  
  
  setwd(eprime_stim_dir)
  hdr = "Weight\tNested\tProcedure\tSoundFile\tCorrectResponse\tPitchLevel\tVowelID\tABS_VowelID\tEuclideanDistance\t"
  header = noquote(hdr)
  # ABS_VowelID is to record the actual vowel (i=1, e =2, etc.) where the vowel # will otherwise vary due to counterbalancing

  # Write header for single vowel list (excludes euclidean distance variable)
  hdr_single = "Weight\tNested\tProcedure\tSoundFile\tCorrectResponse\tPitchLevel\tVowelID\tABS_VowelID\t"
  header_single = noquote(hdr_single)
  write(header_single, file = sprintf("E-prime_Single_Vowel_List_VERSION_%s.txt", version), append = FALSE)
  
  
  
  
  ######################
  #
  # 4. Loop through vowel and pitch options and create .wav files
  #
  ######################
  
  
  setwd(singlevdir)
  
  for(k in 1:nvowels) # Loop through each vowel
  { 
    
    for(j in 1:npitch) # Loop through each pitch
    {
      
      # currVowel = vowelsynth (ffs = FFmatrix[k,], 
      #                          dur = duration, f0 = c(pitchmatrix[j,1], pitchmatrix[j,2]))
      
      
      # Steady state vowel
      currVowel = vowelsynth (ffs = FFmatrix[k,], 
                              dur = duration, f0 = c(pitchmatrix[j,1], pitchmatrix[j,1]))
      
      
      
      
      ######################
      
      
      #### Getting ready to add waveforms together ###### 
      # Create variables with the same names for the workspace
      assign(sprintf("singlevowel_vowel%s_pitch%s", k, j), currVowel)
      
      
      # Get absolute vowel ID 
      abs_vowelID = vowellabels[k] 
      
      # Writes a file saving the currVowel (e.g., "vowel1_pitch3.wav)
      writesound(currVowel,filename = sprintf("singlevowel_vowel%s_%s_pitch%s.wav", k, abs_vowelID, j), fs = 10000)
      
  
      
      # In Praat, combine manipulation object with new pitch tier and save single vowel stimuli
      ########
      
      
      # Normalize amplitude, using PraatR (Scale intensity)
      praat("Scale intensity...", arguments=list(dB), input=SingleVPath(sprintf("singlevowel_vowel%s_%s_pitch%s.wav", k, abs_vowelID, j)), output=DBSingleVPath(sprintf("singlevowel_vowel%s_%s_pitch%s.wav", k, abs_vowelID,  j)), filetype="WAV", overwrite=TRUE )
      
      
      
      
      #################
      # Write to logfile (new line)
      
      setwd(eprime_stim_dir)
      CorrectResponse = noquote(sprintf("%s", k))
      PitchLevel = noquote(sprintf("%s", j))
      VowelID = noquote(sprintf("%s", k))
      procedure = noquote(singlevowelprocedure)
      abs_vowelID = vowellabels[k]  # Keep track of what vowel is created (as counterbalancing affects vowel numbering)
      currSoundFile = noquote(sprintf("singlevowel_vowel%s_%s_pitch1.wav", k, abs_vowelID, j))
      
      
      line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\n", procedure, currSoundFile, CorrectResponse, PitchLevel, VowelID, abs_vowelID)
      write(line, file = sprintf("E-prime_Single_Vowel_List_VERSION_%s.txt", version), append = TRUE)
      
      
      
      
      
      setwd(singlevdir)
      
      
    } # end pitch forloop
    
    # Create spectrogram for each vowel and save it as a PDF (http://www.cookbook-r.com/Graphs/Output_to_a_file/)
    setwd(spectrogram_dir)
    
    
    pdf(sprintf("vowel%s_spectrogram", k))
    plot(spectrogram (currVowel), main = sprintf("Vowel %s: /%s/ Spectrogram", k, vowelIDs[k]))
    dev.off()
    
    
    # Create a waveform for each vowel and save it as a PDF
    # setwd("/Users/michellecohn/Desktop/Stimuli/Waveforms")
    #  pdf(sprintf("vowel%s_waveform", k))
    # plot(currVowel, main = sprintf("Vowel %s: /%s/ Waveform", k, vowelIDs[k]), ylab = "Amplitude")
    # dev.off()
    
    
    # Change working directory back to Stimuli folder
    setwd(singlevdir)
    
  } # end vowel forloop
  
  
  
  ##############################################################
  #
  #
  # 4. Get all soundfile names (to use in E-prime)
  #
  ########
  
  # filenames = noquote(list.files(path = "/Users/michellecohn/Desktop/Stimuli/Single_vowels/", pattern = ".wav"))
  
  # write(filenames, "/Users/michellecohn/Desktop/Stimuli/Single_vowels/Sound_names_LOG.txt", sep="\t")
  
  
  
  
  
  #####################
  # 
  # 4. Create a textfile load in all vowel stim names (.wav) and attributes for E-prime StimList
  #
  #####################
  
  setwd(eprime_stim_dir)
  hdr = "Weight\tNested\tProcedure\tSoundFile\tCorrectResponse\tPitchLevel\tVowel1ID\tVowel2ID\tABS_Vowel1ID\tABS_Vowel2ID\tEuclidean_Distance\tBothVowelsCorrect\tOnlyOneVowelACC\tNVowelsDetected\tConstituentCorrect\t"
  header = noquote(hdr)
  write(header, file = sprintf("E-prime_Double_Vowel_List_VERSION_%s.txt", version), append = FALSE)
  
  
  # Create a new .txt file for each pitch level (to later randomly select from in the practice trials)
  for(z in 1:npitch){
    
    write(header, file = sprintf("E-prime_Practice_Pitch%s_List_VERSION_%s.txt", z, version), append = FALSE)
    
  } #end for
  
  
  
  ##############################################################
  #
  #
  # 5. Read in all dB-adjusted .wav files and combine them to create double vowels
  #
  ########
  
  setwd(dbsinglevdir) # Set working directory to the adjusted single vowels
  
  
  for(q in 1:nvowels){ # loop through all vowels for 1st vowel, always at 120 Hz
    
    for(r in 1:nvowels){ # loop through all vowels for 2nd vowel, pitch levels 1-6
      
      
      for(s in 1:npitch){
        
        if(q!=r){  # if the vowels are not the same, add them together at each pitch level
          
          
          # Get the current vowel IDs
          abs_vowel1ID = vowellabels[q] 
          abs_vowel2ID = vowellabels[r]
          
          vowel1 = loadsound(noquote(sprintf("singlevowel_vowel%s_%s_pitch1.wav", q, abs_vowel1ID)))
          vowel2 = loadsound(noquote(sprintf("singlevowel_vowel%s_%s_pitch%s.wav", r, abs_vowel2ID, s)))
          
          currComb = vowel1$sound + vowel2$sound
          
          # Calculate and log euclidean distance 
          FFvowel1 = FFmatrix[q,] # Formant frequencies for first vowel
          FFvowel2 = FFmatrix[r,] # Formant frequencies for second vowel
          
          vowel1_F1 = FFvowel1[1] # First formant frequency for vowel 1
          vowel2_F1 = FFvowel2[1] # First formant frequency for vowel 2
          
          vowel1_F2 = FFvowel1[2] # Second formant frequency for vowel 1
          vowel2_F2 = FFvowel2[2] # Second formant frequency for vowel 2
          
          
          euclidean = sqrt ( ((vowel1_F1-vowel2_F1)^(2)) + ((vowel1_F2-vowel2_F2)^2)   )
          
          
          abs_vowel1ID = vowellabels[q]  # Keep track of what vowel is created (as counterbalancing affects vowel numbering)
          abs_vowel2ID = vowellabels[r]
          
          setwd(doublevdir) # Write the combined sounds to the double V directory (not dB-adjusted)
          writesound(currComb, filename = sprintf("doublevowel_vowel%s_%s_pitch1+vowel%s_%s_pitch%s.wav", q, abs_vowel1ID, r, abs_vowel2ID, s))
          
          # Normalize amplitude, using PraatR (Scale intensity)
          # And save using vowel identifiers (I, AE, etc.)
          
          
          
          praat("Scale intensity...", arguments=list(dB), input=DoubleVPath(sprintf("doublevowel_vowel%s_%s_pitch1+vowel%s_%s_pitch%s.wav", q, abs_vowel1ID, r, abs_vowel2ID, s)), output=DBDoubleVPath(sprintf("doublevowel_vowel%s_%s_pitch1+vowel%s_%s_pitch%s.wav", q, abs_vowel1ID, r, abs_vowel2ID, s)), filetype="WAV", overwrite=TRUE )
          
          
          
          ### 
          # Write to logfile (new line)
          setwd(eprime_stim_dir)
          currSoundFile = noquote(sprintf("doublevowel_vowel%s_%s_pitch1+vowel%s_%s_pitch%s.wav", q, abs_vowel1ID, r, abs_vowel2ID, s))
          CorrectResponse = noquote(sprintf("%s,%s", q, r))
          PitchLevel = noquote(sprintf("%s", s))
          Vowel1ID = noquote(sprintf("%s", q))
          Vowel2ID = noquote(sprintf("%s", r))
          procedure = noquote(doublevowelprocedure)
          
          
          
          
          line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", procedure, currSoundFile, CorrectResponse, PitchLevel, Vowel1ID, Vowel2ID, abs_vowel1ID, abs_vowel2ID, euclidean)
          write(line, file = sprintf("E-prime_Double_Vowel_List_VERSION_%s.txt", version), append = TRUE)
          
          
          setwd(dbsinglevdir) # Set working directory to the adjusted single vowels
          
          
          # Save information for each pitch level
          if(s == 1){
            setwd(eprime_stim_dir)
            line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", procedure, currSoundFile, CorrectResponse, PitchLevel, Vowel1ID, Vowel2ID, abs_vowel1ID, abs_vowel2ID, euclidean)
            write(line, file = sprintf("E-prime_Practice_Pitch1_List_VERSION_%s.txt", version), append = TRUE)
            
          } #endif
          
          
          if(s == 2){
            setwd(eprime_stim_dir)
            line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", procedure, currSoundFile, CorrectResponse, PitchLevel, Vowel1ID, Vowel2ID, abs_vowel1ID, abs_vowel2ID, euclidean)
            write(line, file = sprintf("E-prime_Practice_Pitch2_List_VERSION_%s.txt", version), append = TRUE)
          } #endif
          
          
          if(s == 3){
            setwd(eprime_stim_dir)
            line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", procedure, currSoundFile, CorrectResponse, PitchLevel, Vowel1ID, Vowel2ID, abs_vowel1ID, abs_vowel2ID, euclidean)
            write(line, file = sprintf("E-prime_Practice_Pitch3_List_VERSION_%s.txt", version), append = TRUE)
          } #endif
          
          
          if(s == 4){
            setwd(eprime_stim_dir)
            line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", procedure, currSoundFile, CorrectResponse, PitchLevel, Vowel1ID, Vowel2ID, abs_vowel1ID, abs_vowel2ID, euclidean)
            write(line, file = sprintf("E-prime_Practice_Pitch4_List_VERSION_%s.txt", version), append = TRUE)
          }#endif
          
          if(s == 5){
            setwd(eprime_stim_dir)
            line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", procedure, currSoundFile, CorrectResponse, PitchLevel, Vowel1ID, Vowel2ID, abs_vowel1ID, abs_vowel2ID, euclidean)
            write(line, file = sprintf("E-prime_Practice_Pitch5_List_VERSION_%s.txt", version), append = TRUE)
            
          } #endif
          
          if(s == 6){
            setwd(eprime_stim_dir)
            line = sprintf("1\t\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", procedure, currSoundFile, CorrectResponse, PitchLevel, Vowel1ID, Vowel2ID, abs_vowel1ID, abs_vowel2ID, euclidean)
            write(line, file = sprintf("E-prime_Practice_Pitch6_List_VERSION_%s.txt", version), append = TRUE)
            
          }#endif
          
          
        }#endif for (q!=r) 
        
        
        setwd(dbsinglevdir) # Set working directory to the adjusted single vowels
        
        
      } # pitch loop
      
    }  #endfor 2nd vowel
    
  }#endfor 1st vowel
  
  
} #endfor version loop (e.g., "A")


# Plot pitch levels 
library(ggplot2)

pitchlevels = as.data.frame(pitchmatrix)
ggplot(data = pitchlevels) + geom_line(aes(colour = pitchlevels$V1))
