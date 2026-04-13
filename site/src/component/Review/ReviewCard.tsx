'use client';
import './ReviewCard.scss';
import { FC, useState, useEffect, useCallback, ReactNode } from 'react';
import { Chip, Tooltip } from '@mui/material';
import { CourseGQLData, ProfessorGQLData } from '../../types/types';
import ReportForm from '../ReportForm/ReportForm';
import { selectReviews, setReviews } from '../../store/slices/reviewSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import ReviewForm from '../ReviewForm/ReviewForm';
import trpc from '../../trpc';
import { ReviewData } from '@peterportal/types';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';
import { sortTerms } from '../../helpers/util';
import { getProfessorTerms } from '../../helpers/reviews';
import { useProfessorData } from '../../hooks/professorReviews';

import PersonIcon from '@mui/icons-material/Person';
import VerifiedUserIcon from '@mui/icons-material/VerifiedUser';
import {
  Button,
  IconButton,
  Card,
  Skeleton,
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  DialogContentText,
  Menu,
  MenuItem,
  Divider,
} from '@mui/material';
import MoreVertIcon from '@mui/icons-material/MoreVert';
import Link from 'next/link';
import { createTooltipOffset } from '../../helpers/slotProps';
import { addPreview } from '../../store/slices/previewSlice';
import { useCurrentPreview } from '../../hooks/preview';

interface AuthorEditButtonsProps {
  review: ReviewData;
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
}

const ThreeDotsMenu: FC<AuthorEditButtonsProps> = ({ review, course, professor }) => {
  const [anchorEl, setAnchorEl] = useState<null | HTMLElement>(null);
  const [showDeleteModal, setShowDeleteModal] = useState(false);
  const [showReviewForm, setShowReviewForm] = useState(false);
  const [reportFormOpen, setReportFormOpen] = useState(false);
  const open = Boolean(anchorEl);

  const dispatch = useAppDispatch();
  const reviewData = useAppSelector(selectReviews);

  const sortedTerms: string[] = sortTerms(course?.terms || (professor ? getProfessorTerms(professor) : []));

  const handleMenuOpen = (e: React.MouseEvent<HTMLElement>) => {
    setAnchorEl(e.currentTarget);
  };

  const handleMenuClose = () => {
    setAnchorEl(null);
  };

  const deleteReview = async (reviewId: number) => {
    await trpc.reviews.delete.mutate({ id: reviewId });
    dispatch(setReviews(reviewData.filter((review) => review.id !== reviewId)));
    setShowDeleteModal(false);
  };

  const openReviewForm = () => {
    setShowReviewForm(true);
    handleMenuClose();
    document.body.style.overflow = 'hidden';
  };

  const closeReviewForm = () => {
    setShowReviewForm(false);
    document.body.style.overflow = 'visible';
  };

  const openReportForm = () => {
    setReportFormOpen(true);
    handleMenuClose();
  };

  return (
    <>
      <IconButton onClick={handleMenuOpen}>
        <MoreVertIcon />
      </IconButton>
      <Menu anchorEl={anchorEl} open={open} onClose={handleMenuClose}>
        <MenuItem onClick={openReportForm}>Report</MenuItem>
        {review.authored && <MenuItem onClick={openReviewForm}>Edit</MenuItem>}
        {review.authored && (
          <MenuItem
            onClick={() => {
              setShowDeleteModal(true);
              handleMenuClose();
            }}
          >
            Delete
          </MenuItem>
        )}
      </Menu>

      <Dialog open={showDeleteModal} onClose={() => setShowDeleteModal(false)} fullWidth>
        <DialogTitle>Delete Review</DialogTitle>
        <DialogContent>
          <DialogContentText>
            Deleting a review will remove it permanently. Are you sure you want to proceed?
          </DialogContentText>
        </DialogContent>
        <DialogActions>
          <Button color="inherit" onClick={() => setShowDeleteModal(false)}>
            Cancel
          </Button>
          <Button color="error" onClick={() => deleteReview(review.id!)}>
            Delete
          </Button>
        </DialogActions>
      </Dialog>

      <ReviewForm
        course={course}
        professor={professor}
        reviewToEdit={review}
        closeForm={closeReviewForm}
        show={showReviewForm}
        editing
        terms={sortedTerms}
      />

      <ReportForm
        showForm={reportFormOpen}
        reviewId={review.id}
        reviewContent={review.content}
        closeForm={() => setReportFormOpen(false)}
      />
    </>
  );
};

interface ReviewCardProps {
  review: ReviewData;
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
  children?: ReactNode;
}

const ReviewCard: FC<ReviewCardProps> = ({ review, course, professor }) => {
  const dispatch = useAppDispatch();
  const reviewData = useAppSelector(selectReviews);
  const isLoggedIn = useIsLoggedIn();
  const [identifier, setIdentifier] = useState<ReactNode>(null);
  const [loadingIdentifier, setLoadingIdentifier] = useState<boolean>(true);
  const [reportFormOpen, setReportFormOpen] = useState<boolean>(false);
  const profCache = useProfessorData(review.professorId);

  const fetchCourseAndProfName = useCallback(async () => {
    let profName: string | undefined = undefined;
    let courseName: string | undefined = undefined;

    try {
      // if cache does not need to be loaded/is empty
      if (!profCache) {
        return;
      }
      const nameParts = profCache.name.split(' ');
      const profInitial = nameParts[0][0] + '.';
      const profLastName = nameParts[nameParts.length - 1];
      profName = `${profInitial} ${profLastName}`;

      const matchedCourse = profCache.courses[review.courseId];

      // first, try to match a course name using the professor's API course array. otherwise, lookup the course separately.
      if (matchedCourse) {
        courseName = `${matchedCourse.department} ${matchedCourse.courseNumber}`;
      } else {
        try {
          const courseResponse = await trpc.courses.get.query({ courseID: review.courseId });
          courseName = `${courseResponse.department} ${courseResponse.courseNumber}`;
        } catch (error) {
          console.error('Error fetching course name: ', error);
        }
      }
      return { courseName, profName };
    } catch (error) {
      console.error('Error fetching professor or course name:', error);
    }
  }, [review.courseId, profCache]);

  const currentPreview = useCurrentPreview();
  const handleLinkClick = useCallback(
    (event: React.MouseEvent, id: string) => {
      if (!currentPreview) return;
      event.preventDefault();
      if (course) {
        dispatch(addPreview({ type: 'instructor', id }));
      } else {
        dispatch(addPreview({ type: 'course', id }));
      }
    },
    [currentPreview, course, dispatch],
  );

  const formatQuarter = (quarter: string): string => {
    const [year, term] = quarter.split(' ');
    const termMap: Record<string, string> = {
      Fall: 'F',
      Winter: 'W',
      Spring: 'Sp',
      Summer: 'Su',
    };
    return `${termMap[term] ?? term}${year.slice(-2)}`;
  };

  useEffect(() => {
    // if loading then return
    if (!profCache) {
      return;
    }

    const getIdentifier = async () => {
      setLoadingIdentifier(true);

      if (professor) {
        const foundCourse = professor.courses[review.courseId];
        const courseName = foundCourse ? `${foundCourse.department} ${foundCourse.courseNumber}` : review.courseId;
        const courseLink = (
          <span>
            <Link
              href={{ pathname: `/course/${encodeURIComponent(review.courseId)}` }}
              onClick={(e) => handleLinkClick(e, review.courseId)}
            >
              {courseName}
            </Link>
            {' • '}
            {formatQuarter(review.quarter)}
          </span>
        );
        setIdentifier(courseLink);
      } else if (course) {
        const foundProf = course.instructors[review.professorId];
        const profName = foundProf ? `${foundProf.name}` : review.professorId;
        const profLink = (
          <span>
            <Link
              href={{ pathname: `/instructor/${review.professorId}` }}
              onClick={(e) => handleLinkClick(e, review.professorId)}
            >
              {profName}
            </Link>
            {' • '}
            {formatQuarter(review.quarter)}
          </span>
        );
        setIdentifier(profLink);
      } else {
        const foundCourseAndProfName = await fetchCourseAndProfName();
        const courseName = foundCourseAndProfName?.courseName ?? review.courseId;
        const profName = foundCourseAndProfName?.profName ?? review.professorId;
        const courseAndProfLink = (
          <span>
            <Link href={{ pathname: `/course/${encodeURIComponent(review.courseId)}` }}>{courseName}</Link>
            {' • '}
            <Link href={{ pathname: `/instructor/${review.professorId}` }}>{profName ?? review.professorId}</Link>
            {' • '}
            {formatQuarter(review.quarter)}
          </span>
        );
        setIdentifier(courseAndProfLink);
      }
      setLoadingIdentifier(false);
    };

    getIdentifier();
  }, [
    course,
    review.courseId,
    review.quarter,
    professor,
    review.professorId,
    fetchCourseAndProfName,
    profCache,
    handleLinkClick,
  ]);

  const updateScore = (newUserVote: number) => {
    dispatch(
      setReviews(
        reviewData.map((otherReview) => {
          if (otherReview.id === review.id) {
            return {
              ...otherReview,
              score: otherReview.score + (newUserVote - otherReview.userVote!),
              userVote: newUserVote,
            };
          } else {
            return otherReview;
          }
        }),
      ),
    );
  };

  const upvote = async () => {
    const newVote = review.userVote === 1 ? 0 : 1;
    await vote(newVote);
  };

  const downvote = async () => {
    const newVote = review.userVote === -1 ? 0 : -1;
    await vote(newVote);
  };

  const vote = async (newVote: number) => {
    updateScore(newVote);
    try {
      await trpc.reviews.vote.mutate({ id: review.id, vote: newVote });
    } catch (err) {
      updateScore(review.userVote);
      console.error('Error sending downvote:', err);
    }
  };

  const openReportForm = () => {
    setReportFormOpen(true);
  };

  const upvoteClassname = review.userVote === 1 ? 'upvote colored-upvote' : 'upvote';
  const downvoteClassname = review.userVote === -1 ? 'downvote colored-downvote' : 'downvote';

  const tooltipProps = {
    placement: 'top' as const,
    slotProps: createTooltipOffset(0, -10),
  };

  const verifiedIcon = (
    <Tooltip title="This review was verified by an administrator." {...tooltipProps}>
      <VerifiedUserIcon />
    </Tooltip>
  );

  const authorIcon = (
    <Tooltip title="You are the author of this review." {...tooltipProps}>
      <PersonIcon />
    </Tooltip>
  );

  const tags: string[] = review.tags?.slice() ?? [];
  if (review.textbook) tags.unshift('Requires textbook');
  if (review.attendance) tags.unshift('Mandatory attendance');

  return (
    <Card variant="outlined" className="reviewcard">
      <div className="reviewcard-header">
        <div className="reviewcard-header-top">
          <h3 className="reviewcard-identifier">
            {loadingIdentifier ? <Skeleton variant="text" animation="wave" width={210} /> : identifier}
          </h3>
          <ThreeDotsMenu review={review} course={course} professor={professor} />
        </div>
        <Divider />
        <div className="reviewcard-header-bottom">
          <div className="reviewcard-author">
            <span className="reviewcard-author-name">{review.userDisplay}</span>
            {review.verified && <div className="reviewcard-author-verified">{verifiedIcon}</div>}
            {review.authored && <div className="reviewcard-author-author">{authorIcon}</div>}
          </div>
          <span className="reviewcard-date">
            {new Date(review.createdAt).toLocaleString('default', {
              year: 'numeric',
              month: '2-digit',
              day: '2-digit',
            })}
            {review.updatedAt && <span className="subtext edit-time"> (edited {new Date().toLocaleDateString()})</span>}
          </span>
        </div>
      </div>

      <div className="reviewcard-content">
        <div className="reviewcard-ratings">
          <div className="rating rating-quality">
            <div className="rating-label">Quality</div>
            <div className="rating-value">{review.rating}</div>
          </div>
          <div className="rating rating-difficulty">
            <div className="rating-label">Difficulty</div>
            <div className="rating-value">{review.difficulty}</div>
          </div>
        </div>
        <div className="reviewcard-info">
          <p>
            Grade: <b>{review.gradeReceived}</b>
          </p>
          <p className="review-content">{review.content || <i>This review has no additional content</i>}</p>
        </div>
      </div>
      {tags.length > 0 && (
        <div className="reviewcard-tags">
          {tags.map((tag) => (
            <Chip size="small" color="primary" key={tag} label={tag} />
          ))}
        </div>
      )}
      <div className="reviewcard-footer" id={review.id.toString()}>
        <div className="reviewcard-voting">
          <p className="reviewcard-voting-question">Helpful?</p>
          <div className="reviewcard-voting-buttons">
            <Tooltip title="You must be logged in to vote" open={isLoggedIn ? false : undefined}>
              <span>
                <button
                  className={upvoteClassname}
                  onClick={upvote}
                  disabled={!isLoggedIn}
                  style={!isLoggedIn ? { pointerEvents: 'none' } : {}}
                >
                  &#9650;
                </button>
              </span>
            </Tooltip>
            <p className="reviewcard-voting-count">{review.score}</p>
            <Tooltip title="You must be logged in to vote" open={isLoggedIn ? false : undefined}>
              <span>
                <button
                  className={downvoteClassname}
                  onClick={downvote}
                  disabled={!isLoggedIn}
                  style={!isLoggedIn ? { pointerEvents: 'none' } : {}}
                >
                  &#9660;
                </button>
              </span>
            </Tooltip>
          </div>
        </div>
        <button className="add-report-button" onClick={openReportForm}>
          Report...
        </button>
        <ReportForm
          showForm={reportFormOpen}
          reviewId={review.id}
          reviewContent={review.content}
          closeForm={() => setReportFormOpen(false)}
        />
      </div>
    </Card>
  );
};

export default ReviewCard;
