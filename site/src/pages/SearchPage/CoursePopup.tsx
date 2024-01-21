import React, { FC, useState, useEffect } from 'react';
import SearchPopup from '../../component/SearchPopup/SearchPopup';
import axios from 'axios';

import { useAppSelector } from '../../store/hooks';
import { selectCourse } from '../../store/slices/popupSlice';
import { ScoreData } from '../../types/types';

const CoursePopup: FC = () => {
  const course = useAppSelector(selectCourse);
  const [scores, setScores] = useState<ScoreData[]>([]);

  useEffect(() => {
    if (course) {
      axios
        .get<ScoreData[]>('/api/reviews/scores', {
          params: {
            type: 'course',
            id: course.id,
          },
        })
        .then((res) => {
          let scores: ScoreData[] = [];
          // set of ucinetid professors with scores
          let scoredProfessors = new Set(res.data.map((v) => v.name));
          // add known scores
          res.data.forEach((entry) => {
            if (course.instructors[entry.name]) {
              scores.push({ name: course.instructors[entry.name].shortenedName, score: entry.score, key: entry.name });
            }
          });
          // add unknown score
          Object.keys(course.instructors).forEach((ucinetid) => {
            if (!scoredProfessors.has(ucinetid)) {
              scores.push({ name: course.instructors[ucinetid].shortenedName, score: -1, key: ucinetid });
            }
          });
          // sort by highest score
          scores.sort((a, b) => b.score - a.score);
          setScores(scores);
        });
    }
  }, [course]);

  if (course) {
    // include prerequisite and restriction panels
    let infos = [
      {
        title: 'Prerequisite',
        content: course.prerequisiteText,
      },
      {
        title: 'Restrictions',
        content: course.restriction,
      },
    ];

    return (
      <SearchPopup
        name={course.id}
        id={course.id}
        title={course.title}
        infos={infos}
        scores={scores}
        searchType="course"
        course={course}
      />
    );
  } else {
    return <SearchPopup name="" id="" infos={[]} scores={[]} searchType="course" title="" />;
  }
};

export default CoursePopup;
