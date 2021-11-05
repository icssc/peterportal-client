import React, { FC } from 'react';
import SearchPopup from '../../component/SearchPopup/SearchPopup';

import { useAppSelector } from '../../store/hooks';
import { selectCourse } from '../../store/slices/popupSlice';
import { useProfessorNames } from '../../hooks/professorNames';

const CoursePopup: FC = () => {
    const course = useAppSelector(selectCourse);
    const { loading, error, professorNames } = useProfessorNames(course && course.id);

    if (course) {
        // include prerequisite and restriction panels
        let infos = [
            {
                title: 'Prerequisite',
                content: course.prerequisite_text
            },
            {
                title: 'Restrictions',
                content: course.restriction
            }
        ]

        // todo: calculate professor scores
        let scores: {name: string; score: number}[] = []!;
        professorNames.forEach(prof => 
            scores.push({
                name: prof.name,
                score: 5
            }))

        return <SearchPopup id={course.id} title={course.title} infos={infos} scores={scores} searchType="course" course={course}/>
    }
    else {
        return <SearchPopup id="" infos={[]} scores={[]} searchType="course" title=""/>
    }
}

export default CoursePopup;