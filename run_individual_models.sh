for i in {1..25}
do
    for seg in '3' '4' '5' '7' '8' 'a' 'b' 'c' 'C' 'd' 'e' 'E' 'f' 'g' 'G' 'h' 'i' 'j' 'k' 'l' 'L' 'm' 'n' 'N' 'o' 'p' 'q' 'r' 's' 'S' 't' 'T' 'u' 'v' 'w' 'x' 'X' 'y' 'z' 'Z'
    do
	for concept in 'NOSE' 'EYE' 'WATER' 'ONE' 'EARTH_\(SOIL\)' 'EAR' 'BONE' 'TONGUE' 'FIRE' 'SEE' 'TWO' 'LEAF' 'I' 'TOOTH' 'HEAD' 'STONE' 'FISH' 'DOG' 'MOON' 'BLOOD' 'DRINK' 'STAR' 'NIGHT' 'NECK' 'NAME' 'WHITE' 'NEW' 'BREAST' 'RED' 'TAIL' 'BLACK' 'SUN' 'LOUSE' 'HAND' 'EAT' 'COME' 'LONG' 'BIRD' 'SMALL' 'BIG'

	do
	    sbatch run_job.sh Rscript run_model_concept_by_seg.R $seg $concept $i
	done
    done
done
