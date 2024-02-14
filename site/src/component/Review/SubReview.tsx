import { FC, useState } from 'react';
import axios from 'axios';
import './Review.scss';
import Badge from 'react-bootstrap/Badge';
import Tooltip from 'react-bootstrap/Tooltip';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import { useCookies } from 'react-cookie';
import { Link } from 'react-router-dom';
import { PersonFill } from 'react-bootstrap-icons';
import { ReviewData, VoteRequest, CourseGQLData, ProfessorGQLData } from '../../types/types';
import ReportForm from '../ReportForm/ReportForm';

interface SubReviewProps {
  review: ReviewData;
  course?: CourseGQLData;
  professor?: ProfessorGQLData;
  userVote: number;
}

const SubReview: FC<SubReviewProps> = ({ review, course, professor, userVote }) => {
  const [score, setScore] = useState(review.score);
  const [cookies] = useCookies(['user']);
  const [vote, setVote] = useState(userVote);
  const [reportFormOpen, setReportFormOpen] = useState<boolean>(false);

  const sendVote = async (voteReq: VoteRequest) => {
    const res = await axios.patch('/api/reviews/vote', voteReq);
    return res.data.deltaScore;
  };

  const upvote = async () => {
    if (cookies.user === undefined) {
      alert('You must be logged in to vote.');
      return;
    }

    const voteReq = {
      id: review._id!,
      upvote: true,
    };

    let newVote;
    let newScore;
    if (vote === 1) {
      newVote = 0;
      newScore = score - 1;
    } else if (vote === 0) {
      newVote = 1;
      newScore = score + 1;
    } else {
      newVote = 1;
      newScore = score + 2;
    }
    setScore(newScore);
    setVote(newVote);
    await sendVote(voteReq).catch((err) => {
      console.error('Error sending upvote:', err);
      setScore(score);
      setVote(vote);
    });
  };

  const downvote = async () => {
    if (cookies.user === undefined) {
      alert('You must be logged in to vote.');
      return;
    }
    const voteReq = {
      id: review._id!,
      upvote: false,
    };

    let newVote;
    let newScore;
    if (vote === 1) {
      newVote = -1;
      newScore = score - 2;
    } else if (vote === 0) {
      newVote = -1;
      newScore = score - 1;
    } else {
      newVote = 0;
      newScore = score + 1;
    }
    setScore(newScore);
    setVote(newVote);
    await sendVote(voteReq).catch((err) => {
      console.error('Error sending downvote:', err);
      setScore(score);
      setVote(vote);
    });
  };

  const openReportForm = () => {
    setReportFormOpen(true);
  };

  const badgeOverlay = <Tooltip id="verified-tooltip">This review was verified by an administrator.</Tooltip>;
  const authorOverlay = <Tooltip id="authored-tooltip">You are the author of this review.</Tooltip>;

  const upvoteClassname = vote === 1 ? 'upvote coloredUpvote' : 'upvote';
  const downvoteClassname = vote === -1 ? 'downvote coloredDownvote' : 'downvote';

  const verifiedBadge = (
    <OverlayTrigger overlay={badgeOverlay}>
      <Badge variant="primary">Verified</Badge>
    </OverlayTrigger>
  );

  const authorBadge = (
    <OverlayTrigger overlay={authorOverlay}>
      <PersonFill size={25} fill="green"></PersonFill>
    </OverlayTrigger>
  );

  return (
    <div className="subreview">
      <div>
        <h3 className="subreview-identifier">
          {professor && (
            <Link to={{ pathname: `/course/${review.courseID}` }}>
              {professor.courses[review.courseID].department + ' ' + professor.courses[review.courseID].courseNumber}
            </Link>
          )}
          {course && (
            <Link to={{ pathname: `/professor/${review.professorID}` }}>
              {Object.values(course.instructors)?.find(({ ucinetid }) => ucinetid === review.professorID)?.name}
            </Link>
          )}
          {!course && !professor && (
            <div>
              {review.courseID} {review.professorID}
            </div>
          )}
        </h3>
      </div>
      <div className="subreview-content">
        <div className="subreview-ratings">
          <div className={'r' + Math.floor(review.rating).toString() + ' rating'}>
            <div className="rating-label">QUALITY</div>
            <div>{review.rating}</div>
          </div>
          <div className={'r' + (6 - Math.floor(review.difficulty)).toString() + ' rating'}>
            <div className="rating-label">DIFFICULTY</div>
            <div>{review.difficulty}</div>
          </div>
        </div>
        <div className="subreview-info">
          <div className="subreview-details">
            <div className="subreview-detail">
              <p>
                Attendance: <b>{review.attendance ? 'Mandatory' : 'Not Mandatory'} </b>
              </p>
              <p>
                Would Take Again: <b>{review.takeAgain ? 'Yes' : 'No'}</b>
              </p>
              <p>
                Textbook: <b>{review.textbook ? 'Yes' : 'No'}</b>
              </p>
            </div>
            <div className="subreview-detail">
              <p>
                Quarter Taken: <b>{review.quarter}</b>
              </p>
              <p>
                Grade Received: <b>{review.gradeReceived}</b>
              </p>
            </div>
          </div>
          <div>
            <div className="subreview-author">
              <p className=" gapped">
                <span className=" mr-1">Posted by {review.userDisplay}</span>
                {review.verified && verifiedBadge}
                {cookies.user?.id === review.userID && authorBadge}
              </p>
              <p>
                {new Date(review.timestamp).toLocaleString('default', {
                  year: 'numeric',
                  month: 'long',
                  day: 'numeric',
                })}
              </p>
            </div>
            <p>{review.reviewContent}</p>
          </div>
        </div>
      </div>
      <div>
        {review.tags?.map((tag) => (
          <Badge pill className="p-3 mr-2 mt-2" variant="info" key={tag}>
            {tag}
          </Badge>
        ))}
      </div>
      <div className="subreview-footer" id={review._id}>
        <p>Helpful?</p>
        <button className={upvoteClassname} onClick={upvote}>
          &#9650;
        </button>
        <p>{score}</p>
        <button className={downvoteClassname} onClick={downvote}>
          &#9660;
        </button>
        <button type="button" className="add-report-button" onClick={openReportForm}>
          Report
        </button>
        <ReportForm
          showForm={reportFormOpen}
          reviewID={review._id}
          reviewContent={review.reviewContent}
          closeForm={() => setReportFormOpen(false)}
        />
      </div>
    </div>
  );
};

export default SubReview;
