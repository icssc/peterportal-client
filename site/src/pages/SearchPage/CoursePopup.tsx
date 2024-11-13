import { FC, useState, useEffect } from 'react';
import SearchPopup from '../../component/SearchPopup/SearchPopup';

import { useAppSelector } from '../../store/hooks';
import { selectCourse } from '../../store/slices/popupSlice';
import { ScoreData } from '../../types/types';
import trpc from '../../trpc';

const CoursePopup: FC = () => {
  const course = useAppSelector(selectCourse);
  const [scores, setScores] = useState<ScoreData[]>([]);

  useEffect(() => {
    if (course) {
      trpc.reviews.avgRating.query({ type: 'course', id: course.id }).then((res) => {
        const scores: ScoreData[] = [];
        // set of ucinetid professors with scores
        const scoredProfessors = new Set(res.map((v) => v.name));
        // add known scores
        res.forEach((entry) => {
          if (course.instructors[entry.name]) {
            scores.push({
              name: course.instructors[entry.name].name,
              avgRating: entry.avgRating,
              id: entry.name,
            });
          }
        });
        // add unknown score
        Object.keys(course.instructors).forEach((ucinetid) => {
          if (!scoredProfessors.has(ucinetid)) {
            scores.push({ name: course.instructors[ucinetid].name, avgRating: -1, id: ucinetid });
          }
        });
        // sort by highest score
        scores.sort((a, b) => b.avgRating - a.avgRating);
        setScores(scores);
      });
    }
  }, [course]);

  if (course) {
    // include prerequisite and restriction panels
    const infos = [
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
