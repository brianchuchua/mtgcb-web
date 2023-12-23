import { useEffect, useState } from 'react';

export const useConfetti = (isFetching: boolean, percentageCollected: number) => {
  const [countOfApiCalls, setCountOfApiCalls] = useState(0);
  useEffect(() => {
    if (isFetching) {
      setCountOfApiCalls(countOfApiCalls + 1);
    }
  }, [isFetching]);

  const [confettiTriggered, setConfettiTriggered] = useState(false);

  useEffect(() => {
    if (percentageCollected === 100 && countOfApiCalls > 1) {
      setConfettiTriggered(true);
    } else {
      setConfettiTriggered(false);
    }
  }, [percentageCollected]);

  return confettiTriggered;
};
