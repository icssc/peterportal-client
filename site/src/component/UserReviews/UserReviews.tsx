import axios, { AxiosResponse } from "axios";
import React, { FC, useEffect, useState } from "react";
import SubReview from "../../component/Review/SubReview";
import Button from "react-bootstrap/Button";
import { Divider, Modal } from "semantic-ui-react";
import { CourseGQLData, ProfessorGQLData, ReviewData } from "src/types/types";
import "./UserReviews.scss";
import { useCookies } from "react-cookie";
import { Interface } from "readline";
import { searchAPIResult } from "src/helpers/util";


const UserReviews: FC = () => {
  const [reviews, setReviews] = useState<ReviewData[]>([]);
  const [loaded, setLoaded] = useState<boolean>(false);
  const [cookies, setCookie] = useCookies(["user"]);
  const [showModal, setShowModal] = useState<boolean>(false);
  const [selectedReviewId, setSelectedReviewId] = useState<string>("");
  const [professorData, setProfessorData] = useState<Map<string, ProfessorGQLData>>(new Map());
  const [courseData, setCourseData] = useState<Map<string, CourseGQLData>>(new Map());
  const getUserReviews = async () => {
    const response: AxiosResponse<ReviewData[]> = await axios.get(
      `/reviews?userID=${cookies.user.id}`
    );
    setReviews(response.data);
    setLoaded(true);
  };

  useEffect(() => {
    getUserReviews()
  }, []);

  useEffect(() => {
    // wrap in async function as use awaits so we don't
    // update the state until after we populated the maps
    // with results from the api request
    (async() => {
      const newCourseData = new Map(courseData);
      const newProfessorData = new Map(professorData);
      for (const review of reviews) {
        const courseID = review.courseID;
        if (!courseData.has(courseID)) {
          await searchAPIResult('course', courseID)
            .then(course => {
                if (course) {
                    newCourseData.set(courseID, (course as CourseGQLData));
                }
                else {
                    console.log(`Course ${courseID} does not exist!`);
                }
            });
        }

        const profId = review.professorID;
        if (!professorData.has(profId)) {
          await searchAPIResult('professor', profId)
            .then(professor => {
                if (professor) {
                    newProfessorData.set(profId, (professor as ProfessorGQLData));
                }
                else {
                    console.log(`Professor ${profId} does not exist!`);
                }
            });
        }

        setCourseData(newCourseData);
        setProfessorData(newProfessorData);
      }
    })();
  }, [reviews]);

  const deleteReview = async (reviewID: string) => {
    await axios.delete("/reviews", { data: { id: reviewID } });
    setReviews(reviews.filter((review) => review._id !== reviewID));
    setShowModal(false);
  };

  const deleteMsgConfirm = (reviewID: string): void => {
    setSelectedReviewId(reviewID);
    setShowModal(true);
  };

  const handleDeleteReview = () => {
    deleteReview(selectedReviewId);
  };

  if (!loaded) {
    return <p>Loading...</p>;
  } else if (reviews.length === 0) {
    return <p>No reviews to display at the moment.</p>;
  } else {
    return (
      <div className="user-reviews-container">
        <h1>Your Reviews</h1>
        <p>Deleting a review will remove it permanently.</p>
        {reviews.map((review, i) => (
          <div key={`${review._id}`} className="user-reviews">
            <Divider />
            <SubReview editable={true} review={review} course={courseData.get(review.courseID)} professor={professorData.get(review.professorID)}></SubReview>
            <div className="user-reviews-footer">
              <Button
                variant="danger"
                className="mr-3"
                onClick={() => deleteMsgConfirm(review._id!)}
              >
                Delete
              </Button>
            </div>
          </div>
        ))}
        <div className="delete-modal">
          <Modal
            open={showModal}
            onClose={() => setShowModal(false)}
            size="mini"
            style={{
              display: "block",
              position: "initial",
              alignItems: "center",
              justifyContent: "center",
              width: "40%",
              height: "30%",
            }}
          >
            <Modal.Header>Confirm Delete</Modal.Header>
            <Modal.Content>
              <p>Are you sure you want to permanently delete your review?</p>
            </Modal.Content>
            <Modal.Actions>
              <div className="delete-confirmation-buttons">
                <Button onClick={handleDeleteReview} variant="danger">
                  Delete
                </Button>
                <Button onClick={() => setShowModal(false)} variant="primary">
                  Cancel
                </Button>
              </div>
            </Modal.Actions>
          </Modal>
        </div>
      </div>
    );
  }
};

export default UserReviews;
