import React, { FC } from 'react';
import SearchPopup from '../../component/SearchPopup/SearchPopup';

import { useAppSelector } from '../../store/hooks';
import { selectProfessor } from '../../store/slices/popupSlice';

const ProfessorPopup: FC = () => {
    const professor = useAppSelector(selectProfessor);

    if (professor) {
        // include basic info and featured review panels
        let infos = [
            {
                title: 'Basic Info',
                content:`Email: ${professor.ucinetid}@uci.edu`
            },
            {
                title: 'Featured Review',
                content: 'TODO'
            }
        ]        

        // todo: calculate professor scores
        let scores: {name: string; score: number}[] = []!;
        professor.course_history.forEach(course => 
            scores.push({
                name: course,
                score: 5
            }))

        return <SearchPopup id={professor.name} title={professor.title} infos={infos} scores={scores} searchType="professor" professor={professor}/>
    }
    else {
        return <SearchPopup id="" infos={[]} scores={[]} searchType="professor" title=""/>
    }
}

export default ProfessorPopup;