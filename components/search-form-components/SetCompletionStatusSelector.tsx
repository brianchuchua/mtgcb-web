import Paper from '@material-ui/core/Paper';
import ToggleButton from '@material-ui/lab/ToggleButton';
import ToggleButtonGroup from '@material-ui/lab/ToggleButtonGroup';
import styled from 'styled-components';

interface SetCompletionStatusSelectorProps {
  completionStatuses: string[];
  setCompletionStatuses: (completionStatuses: string[]) => void;
}

const SetCompletionStatusSelector: React.FC<SetCompletionStatusSelectorProps> = ({ completionStatuses, setCompletionStatuses }) => {
  const updateCompletionStatuses = (completionStatusesValue) => {
    setCompletionStatuses(completionStatusesValue);
  };

  const handleCompletionStatusChange = (event: React.MouseEvent<HTMLElement>, newCompletionStatuses: string[] | null) => {
    if (newCompletionStatuses != null) {
      if (newCompletionStatuses.length === 3 || (newCompletionStatuses.includes('all') && !completionStatuses.includes('all'))) {
        updateCompletionStatuses(['all']);
      } else if (newCompletionStatuses.includes('all') && newCompletionStatuses.length > 1 && newCompletionStatuses.length < 4) {
        newCompletionStatuses.splice(newCompletionStatuses.indexOf('all'), 1);
        updateCompletionStatuses(newCompletionStatuses);
      } else {
        updateCompletionStatuses(newCompletionStatuses);
      }
    }
  };

  return (
    <ToggleGroupWrapper>
      <StyledToggleButtonGroup
        value={completionStatuses}
        onChange={handleCompletionStatusChange}
        aria-label="select set completion statuses"
      >
        <StyledToggleButton value="all" aria-label="all">
          All
        </StyledToggleButton>
        <StyledToggleButton value="complete" aria-label="complete">
          Complete
        </StyledToggleButton>
        <StyledToggleButton value="partial" aria-label="partial">
          Partial
        </StyledToggleButton>
        <StyledToggleButton value="empty" aria-label="empty">
          Empty
        </StyledToggleButton>
      </StyledToggleButtonGroup>
    </ToggleGroupWrapper>
  );
};

const ToggleGroupWrapper = styled(Paper)(() => ({ margin: '10px', marginTop: '0px', marginBottom: '10px' }));
const StyledToggleButton = styled(ToggleButton)(() => ({ alignItems: 'end', justifyContent: 'center', width: '100%' }));
const StyledToggleButtonGroup = styled(ToggleButtonGroup)(() => ({ width: '100%' }));

export default SetCompletionStatusSelector;
