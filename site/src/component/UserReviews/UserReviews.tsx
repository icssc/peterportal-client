import axios, { AxiosResponse } from "axios";
import React, { FC, useEffect, useState } from "react";
import SubReview from "../../component/Review/SubReview";
import Button from "react-bootstrap/Button";
import { Divider, Modal } from "semantic-ui-react";
import { ReviewData } from "src/types/types";
import "./UserReviews.scss";
import { useCookies } from "react-cookie";

const UserReviews: FC = () => {
  const [reviews, setReviews] = useState<ReviewData[]>([]);
  const [loaded, setLoaded] = useState<boolean>(false);
  const [cookies, setCookie] = useCookies(["user"]);
  const [showModal, setShowModal] = useState<boolean>(false);
  const [selectedReviewId, setSelectedReviewId] = useState<string>("");
  const getUserReviews = async () => {
    const response: AxiosResponse<ReviewData[]> = await axios.get(
      `/reviews?userID=${cookies.user.id}`
    );
    setReviews(response.data);
    setLoaded(true);
  };

  useEffect(() => {
    getUserReviews();
    
  }, []);

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
          <div key={`user-reviews-${i}`} className="user-reviews">
            <Divider />
            <SubReview review={review}></SubReview>
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
