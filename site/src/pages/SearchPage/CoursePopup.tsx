import { FC, useState, useEffect } from 'react';
import { SearchPopup, SearchPopupPlaceholder } from '../../component/SearchPopup/SearchPopup';
import { useAppSelector } from '../../store/hooks';
import { selectCourse } from '../../store/slices/popupSlice';
import { ScoreData } from '../../types/types';
import trpc from '../../trpc';

const CoursePopup: FC = () => {
  const course = useAppSelector(selectCourse);
  const [scores, setScores] = useState<ScoreData[]>([]);

  useEffect(() => {
    if (!course) return;
    trpc.reviews.avgRating.query({ type: 'course', id: course.id }).then((res) => {
      // set of ucinetid professors with scores
      const scoredProfessors = new Set(res.map((v) => v.name));
      const scores: ScoreData[] = [];

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
  }, [course]);

  if (!course) {
    return <SearchPopupPlaceholder dataType="course" />;
  }

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
      dataType="course"
      data={course}
      id={course.id}
      name={course.id}
      title={course.title}
      infos={infos}
      scores={scores}
    />
  );
};

export default CoursePopup;
