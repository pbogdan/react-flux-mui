/* eslint-env mocha */

import React from 'react';
import {assert} from 'chai';
import {shallow} from 'enzyme';
import {spy} from 'sinon';
import AutoComplete from './AutoComplete';
import Menu from '../Menu/Menu';
import Popover from '../Popover/Popover';
import getMuiTheme from '../styles/getMuiTheme';

describe('<AutoComplete />', () => {
  const muiTheme = getMuiTheme();
  const shallowWithContext = (node) => shallow(node, {context: {muiTheme}});

  describe('filter', () => {
    it('search using fuzzy filter', () => {
      assert.strictEqual(AutoComplete.fuzzyFilter('ea', 'Peach'), true, 'should match Peach with ea');
      assert.strictEqual(AutoComplete.fuzzyFilter('pah', 'Peach'), true, 'should match Peach with pah');
      assert.strictEqual(AutoComplete.fuzzyFilter('peach', 'Peach'), true, 'should match Peach with peach');

      assert.strictEqual(AutoComplete.fuzzyFilter('phc', 'Peach'), false, 'should not match Peach with phc');
      assert.strictEqual(AutoComplete.fuzzyFilter('pp', 'Peach'), false, 'should not match Peach with pp');
      assert.strictEqual(AutoComplete.fuzzyFilter('pb', 'Peach'), false, 'should not match Peach with pb');

      // testing longer string
      const test_string = 'The best thing about a Boolean is even if you are wrong, you are only off by a bit.';

      let search_result = AutoComplete.fuzzyFilter('bOOLEAN', test_string);
      assert.strictEqual(search_result, true, 'should match with case insensitive');

      search_result = AutoComplete.fuzzyFilter('The a Boolean if wrong', test_string);
      assert.strictEqual(search_result, true, 'should match pattern with spaces');

      search_result = AutoComplete.fuzzyFilter(' if ,bit.', test_string);
      assert.strictEqual(search_result, true, 'should match pattern with comma and period');

      search_result = AutoComplete.fuzzyFilter('the best q', test_string);
      assert.strictEqual(search_result, false, 'should not match pattern with letter is not contained in search text');

      search_result = AutoComplete.fuzzyFilter('off bit by', 'off by a bit');
      assert.strictEqual(search_result, false, 'should not match pattern when can not find letters in order ');
    });

    /**
     * This test ensures that <AutoComplete /> doesn't pass down filter property to <TextField />,
     * otherwise <TextField /> will render input as <input filter="function (...) {...}" ... />,
     * which will have different behaviors in different environments, producing indent conflicts and
     * breaking server rendering.
     * Read more: https://github.com/callemall/material-ui/issues/4195
     */
    it('should not pass filter property to children', () => {
      const wrapper = shallowWithContext(
        <AutoComplete dataSource={[]} />
      );

      assert.strictEqual(
        wrapper.find('TextField').prop('filter'),
        undefined,
        'should not pass filter property to children');
    });
  });

  describe('prop: onNewRequest', () => {
    it('should call onNewRequest once the popover is closed', (done) => {
      const handleNewRequest = spy();
      const wrapper = shallowWithContext(
        <AutoComplete
          dataSource={['foo', 'bar']}
          searchText="f"
          onNewRequest={handleNewRequest}
          menuCloseDelay={10}
        />
      );

      wrapper.setState({open: true});
      wrapper.find(Menu).props().onItemTouchTap({}, {
        key: 0,
      });
      assert.strictEqual(handleNewRequest.callCount, 0);
      assert.strictEqual(wrapper.state().searchText, 'foo');

      setTimeout(() => {
        assert.strictEqual(handleNewRequest.callCount, 1);
        assert.deepEqual(handleNewRequest.args[0], [
          'foo',
          0,
        ]);
        done();
      }, 20);
    });
  });

  describe('prop: onUpdateInput', () => {
    it('should fire after selection from menu', (done) => {
      const handleUpdateInput = spy();
      const wrapper = shallowWithContext(
        <AutoComplete
          dataSource={['foo', 'bar']}
          searchText="f"
          onUpdateInput={handleUpdateInput}
        />
      );

      wrapper.setState({open: true});
      wrapper.find(Menu).props().onItemTouchTap({}, {
        key: 0,
      });
      assert.strictEqual(wrapper.state().searchText, 'foo');

      setTimeout(() => {
        assert.strictEqual(handleUpdateInput.callCount, 1);
        assert.deepEqual(handleUpdateInput.args[0], [
          'foo',
          [
            'foo',
            'bar',
          ],
          {
            source: 'touchTap',
          },
        ]);
        done();
      }, 0);
    });
  });

  describe('prop: popoverProps', () => {
    it('should pass popoverProps to Popover', () => {
      const wrapper = shallowWithContext(
        <AutoComplete
          dataSource={['foo', 'bar']}
          popoverProps={{
            zDepth: 3,
            canAutoPosition: true,
          }}
        />
      );

      const popoverProps = wrapper.find(Popover).props();

      assert.strictEqual(popoverProps.zDepth, 3, 'should pass popoverProps to Popover');
      assert.strictEqual(popoverProps.canAutoPosition, true, 'should overrides the default');
    });
  });

  describe('prop: onClose', () => {
    it('should call onClose when the menu is closed', () => {
      const handleClose = spy();
      const wrapper = shallowWithContext(
        <AutoComplete dataSource={['foo', 'bar']} onClose={handleClose} />
      );
      wrapper.instance().close();
      assert.strictEqual(handleClose.callCount, 1);
    });
  });
});
