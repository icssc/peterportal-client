import React, { FC, useState, useEffect } from 'react';
import SearchPopup from '../../component/SearchPopup/SearchPopup';
import axios from 'axios'

import { useAppSelector } from '../../store/hooks';
import { selectCourse } from '../../store/slices/popupSlice';
import { ScoreData } from '../../types/types';

const CoursePopup: FC = () => {
    const course = useAppSelector(selectCourse);
    const [scores, setScores] = useState<ScoreData[]>([]);

    useEffect(() => {
        if (course) {
            axios.get<ScoreData[]>('/reviews/scores', {
                params: {
                    type: 'course',
                    id: course.id
                }
            })
                .then(res => {
                    let scoredProfessors = new Set(res.data.map(v => v.name));
                    course.professor_history.forEach(professor => {
                        // add unknown score
                        if (!scoredProfessors.has(professor)) {
                            res.data.push({ name: professor, score: -1 })
                        }
                    })
                    // sort by highest score
                    res.data.sort((a, b) => b.score - a.score);
                    setScores(res.data);
                });
        }
    }, [course])

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

        return <SearchPopup name={course.id} id={course.id} title={course.title} infos={infos} scores={scores} searchType="course" course={course} />
    }
    else {
        return <SearchPopup name="" id="" infos={[]} scores={[]} searchType="course" title="" />
    }
}

export default CoursePopup;