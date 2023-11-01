import { FC, useState, useEffect } from 'react';
import SearchPopup from '../../component/SearchPopup/SearchPopup';
import axios from 'axios'

import { useAppSelector } from '../../store/hooks';
import { selectProfessor } from '../../store/slices/popupSlice';
import { ScoreData, ReviewData } from '../../types/types';

const ProfessorPopup: FC = () => {
    const professor = useAppSelector(selectProfessor);
    const [featured, setFeatured] = useState<ReviewData>(null!);
    const [scores, setScores] = useState<ScoreData[]>([]);

    useEffect(() => {
        if (professor) {
            const reviewParams = {
                type: 'professor',
                id: professor.ucinetid
            };
            axios.get<ScoreData[]>('/api/reviews/scores', { params: reviewParams })
                .then(res => {
                    const scoredCourses = new Set(res.data.map(v => v.name));
                    res.data.forEach(v => v.key = v.name)
                    Object.keys(professor.course_history).forEach(course => {
                        // remove spaces
                        course = course.replace(/\s+/g, '');
                        // add unknown score
                        if (!scoredCourses.has(course)) {
                            res.data.push({ name: course, score: -1, key: course })
                        }
                    })
                    // sort by highest score
                    res.data.sort((a, b) => b.score - a.score);
                    setScores(res.data);
                });
            axios.get<ReviewData[]>('/api/reviews/featured', { params: reviewParams })
                .then(res => {
                    // if has a featured review
                    if (res.data.length > 0) {
                        setFeatured(res.data[0]);
                    }
                    // no reviews for this professor
                    else {
                        setFeatured(null!);
                    }
                });
        }
    }, [professor])

    if (professor) {
        // include basic info and featured review panels
        const infos = [
            {
                title: 'Basic Info',
                content: `Email: ${professor.ucinetid}@uci.edu`
            },
            {
                title: `Featured Review`,
                content: featured ? (`For ${featured.courseID}: ${featured.reviewContent}`) : 'No Reviews Yet!'
            }
        ]

        return <SearchPopup name={professor.name} id={professor.ucinetid} title={professor.title} infos={infos} scores={scores} searchType="professor" professor={professor} />
    }
    else {
        return <SearchPopup name="" id="" infos={[]} scores={[]} searchType="professor" title="" />
    }
}

export default ProfessorPopup;