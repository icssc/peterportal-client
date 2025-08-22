import { FC } from 'react';
import './HitItem.scss';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setProfessor } from '../../store/slices/popupSlice';

import { ProfessorGQLData } from '../../types/types';
import { useIsMobile } from '../../helpers/util';
import { useRouter } from 'next/navigation';
import Link from 'next/link';

interface ProfessorHitItemProps extends ProfessorGQLData {}

const ProfessorHitItem: FC<ProfessorHitItemProps> = (props: ProfessorHitItemProps) => {
  const dispatch = useAppDispatch();
  const router = useRouter();
  const isMobile = useIsMobile();
  const activeProfessor = useAppSelector((state) => state.popup.professor);

  const onClickName = () => {
    // set the professor popup
    dispatch(setProfessor(props));

    // if click on a professor that is already in popup
    // or if on mobile
    if ((activeProfessor && props.ucinetid == activeProfessor.ucinetid) || isMobile) {
      router.push(`/professor/${props.ucinetid}`);
    }
  };

  const onKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
    if (e.key === 'Enter') {
      onClickName();
    }
  };

  const initialsText = props.name
    .split(' ')
    .map((x: string) => x[0])
    .join('');
  const initialsSize = 22 - (initialsText.length - 2) * 4;

  return (
    <div className="hit-item professor-hit" tabIndex={0} role="button" onClick={onClickName} onKeyDown={onKeyDown}>
      <div className="name-container">
        <div className="name-icon" style={{ fontSize: initialsSize }}>
          {initialsText}
        </div>
        <div>
          <p className="hit-name">{props.name}</p>
          <p className="hit-subtitle">
            {props.department}&nbsp;• {props.title}
          </p>
        </div>
      </div>
      {Object.keys(props.courses).length > 0 && (
        <div>
          <p>
            <b>Recently taught: </b>
            {Object.keys(props.courses).map((item: string, index: number) => {
              return (
                <span key={`professor-hit-item-course-${index}`}>
                  {index ? ', ' : ''}
                  <Link href={'/course/' + item.replace(/\s+/g, '')} onClick={(e) => e.stopPropagation()}>
                    {item}
                  </Link>
                </span>
              );
            })}
          </p>
        </div>
      )}
    </div>
  );
};

export default ProfessorHitItem;
