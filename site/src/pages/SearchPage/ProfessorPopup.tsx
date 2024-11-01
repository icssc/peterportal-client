import { FC, useState, useEffect } from 'react';
import SearchPopup from '../../component/SearchPopup/SearchPopup';

import { useAppSelector } from '../../store/hooks';
import { selectProfessor } from '../../store/slices/popupSlice';
import { ScoreData } from '../../types/types';
import trpc from '../../trpc';
import { FeaturedReviewData } from '@peterportal/types';

const ProfessorPopup: FC = () => {
  const professor = useAppSelector(selectProfessor);
  const [featured, setFeatured] = useState<FeaturedReviewData>(null!);
  const [scores, setScores] = useState<ScoreData[]>([]);

  useEffect(() => {
    if (professor) {
      const reviewParams = {
        type: 'professor',
        id: professor.ucinetid,
      } as const;
      trpc.reviews.scores.query(reviewParams).then((res: ScoreData[]) => {
        const scoredCourses = new Set(res.map((v) => v.name));
        res.forEach((v) => (v.key = v.name));
        Object.keys(professor.courses).forEach((course) => {
          // remove spaces
          course = course.replace(/\s+/g, '');
          // add unknown score
          if (!scoredCourses.has(course)) {
            res.push({ name: course, score: -1, key: course });
          }
        });
        // sort by highest score
        res.sort((a, b) => b.score - a.score);
        setScores(res);
      });
      trpc.reviews.featured.query(reviewParams).then((res) => {
        // if has a featured review
        if (res) {
          setFeatured(res);
        }
        // no reviews for this professor
        else {
          setFeatured(null!);
        }
      });
    }
  }, [professor]);

  if (professor) {
    // include basic info and featured review panels
    const infos = [
      {
        title: 'Basic Info',
        content: `Email: ${professor.ucinetid}@uci.edu`,
      },
      {
        title: `Featured Review`,
        content: featured
          ? `For ${featured.courseId}: ${(featured.content ?? '').length > 0 ? featured.content : 'Rating of ' + featured.rating + '/5'}`
          : 'No Reviews Yet!',
      },
    ];

    return (
      <SearchPopup
        name={professor.name}
        id={professor.ucinetid}
        title={professor.title}
        infos={infos}
        scores={scores}
        searchType="professor"
        professor={professor}
      />
    );
  } else {
    return <SearchPopup name="" id="" infos={[]} scores={[]} searchType="professor" title="" />;
  }
};

export default ProfessorPopup;
