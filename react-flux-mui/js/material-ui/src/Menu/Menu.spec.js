/* eslint-env mocha */
import React from 'react';
import {mount, shallow} from 'enzyme';
import {spy} from 'sinon';
import {assert} from 'chai';
import Menu from './Menu';
import MenuItem from '../MenuItem';
import Divider from '../Divider';
import getMuiTheme from '../styles/getMuiTheme';
import keycode from 'keycode';

describe('<Menu />', () => {
  const muiTheme = getMuiTheme();
  const shallowWithContext = (node) => shallow(node, {context: {muiTheme}});
  const mountWithContext = (node) => mount(node, {context: {muiTheme}});
  const keycodeEvent = (key) => ({keyCode: keycode(key)});

  describe('onMenuItemFocusChange', () => {
    function createMenu(props) {
      return (
        <Menu {...props}>
          <MenuItem primaryText="item 1" />
          <Divider />
          <MenuItem primaryText="item 2" />
          <MenuItem primaryText="item 3" />
        </Menu>
      );
    }

    it('is invoked when using the arrow key to go down to the bottom and back up to the top', () => {
      const onMenuItemFocusChangeSpy = spy();
      const menu = createMenu({
        disableAutoFocus: false,
        onMenuItemFocusChange: onMenuItemFocusChangeSpy,
      });
      const wrapper = mountWithContext(menu);

      assert.deepEqual(onMenuItemFocusChangeSpy.args[0], [null, 0],
        'initial focus should invoke callback with 0');
      onMenuItemFocusChangeSpy.reset();

      wrapper.simulate('keydown', keycodeEvent('down'));
      assert.strictEqual(onMenuItemFocusChangeSpy.args[0][1], 1,
        'down-arrow invokes callback with index 1');
      onMenuItemFocusChangeSpy.reset();

      wrapper.simulate('keydown', keycodeEvent('down'));
      assert.strictEqual(onMenuItemFocusChangeSpy.args[0][1], 2,
        'down-arrow invokes callback with index 2');
      onMenuItemFocusChangeSpy.reset();

      wrapper.simulate('keydown', keycodeEvent('down'));
      assert.strictEqual(onMenuItemFocusChangeSpy.args[0][1], 2,
        'down-arrow at end invokes callback with unchanged index');
      onMenuItemFocusChangeSpy.reset();

      wrapper.simulate('keydown', keycodeEvent('up'));
      assert.strictEqual(onMenuItemFocusChangeSpy.args[0][1], 1,
        'up-arrow invokes callback with 1');
      onMenuItemFocusChangeSpy.reset();

      wrapper.simulate('keydown', keycodeEvent('up'));
      assert.strictEqual(onMenuItemFocusChangeSpy.args[0][1], 0,
        'up-arrow invokes callback with 0');
      onMenuItemFocusChangeSpy.reset();

      wrapper.simulate('keydown', keycodeEvent('up'));
      assert.strictEqual(onMenuItemFocusChangeSpy.args[0][1], 0,
        'up-arrow at top invokes callback with unchanged index');
      onMenuItemFocusChangeSpy.reset();

      wrapper.unmount(); // Otherwise the timer in FocusRipple keeps Node from exiting
    });

    it('is invoked when props change', () => {
      const onMenuItemFocusChangeSpy = spy();
      const menu = createMenu({
        disableAutoFocus: true,
        onMenuItemFocusChange: onMenuItemFocusChangeSpy,
      });
      const wrapper = mountWithContext(menu);
      assert.strictEqual(onMenuItemFocusChangeSpy.callCount, 0,
        'should not be called when creating with disableAutoFocus=true');
      onMenuItemFocusChangeSpy.reset();

      wrapper.setProps({disableAutoFocus: false});
      assert.deepEqual(onMenuItemFocusChangeSpy.args[0], [null, 0],
        'changing disableAutoFocus to false invokes callback');
      onMenuItemFocusChangeSpy.reset();

      wrapper.setProps({disableAutoFocus: true});
      assert.deepEqual(onMenuItemFocusChangeSpy.args[0], [null, -1],
        'changing disableAutoFocus to true invokes callback');
      onMenuItemFocusChangeSpy.reset();

      wrapper.unmount(); // Otherwise the timer in FocusRipple keeps Node from exiting
    });

    it('is invoked for hotkeys', () => {
      const onMenuItemFocusChangeSpy = spy();
      const menu = (
        <Menu
          disableAutoFocus={false}
          onMenuItemFocusChange={onMenuItemFocusChangeSpy}
        >
          <MenuItem primaryText="a00" />
          <MenuItem primaryText="b11" />
          <Divider />
          <MenuItem primaryText="b00" />
        </Menu>
      );
      const wrapper = mountWithContext(menu);

      wrapper.simulate('keydown', keycodeEvent('b'));
      assert.strictEqual(onMenuItemFocusChangeSpy.args[0][1], 0,
        '"b" invokes callback with focus index 0');
      onMenuItemFocusChangeSpy.reset();

      wrapper.simulate('keydown', keycodeEvent('0'));
      // The Divider is incorrectly counted by Menu.setFocusIndexStartsWith().
      assert.strictEqual(onMenuItemFocusChangeSpy.args[0][1], 3,
        '"b0" invokes callback with focus index 3, which is probably a bug');
      onMenuItemFocusChangeSpy.reset();

      wrapper.simulate('keydown', keycodeEvent('0'));
      assert.strictEqual(onMenuItemFocusChangeSpy.args[0][1], 3,
        '"b0" invokes callback with focus index 3');
      onMenuItemFocusChangeSpy.reset();

      wrapper.simulate('keydown', keycodeEvent('!'));
      // It seems like the focus index should be changed to -1 here.
      assert.strictEqual(onMenuItemFocusChangeSpy.callCount, 0,
        '"b00!" does not change the focus index, which is probably a bug');
      onMenuItemFocusChangeSpy.reset();

      wrapper.unmount(); // Otherwise the timer in FocusRipple keeps Node from exiting
    });
  });

  it('should render MenuItem and Divider children', () => {
    const wrapper = shallowWithContext(
      <Menu>
        <MenuItem primaryText="item 1" />
        <Divider />
        <MenuItem primaryText="item 2" />
      </Menu>
    );

    const menuItemsAndDividers = wrapper.children().children().children();
    assert.strictEqual(menuItemsAndDividers.length, 3, 'there should be three children');
    assert.strictEqual(menuItemsAndDividers.get(0).type, MenuItem, 'first child should be a MenuItem');
    assert.strictEqual(menuItemsAndDividers.get(1).type, Divider, 'second child should be a Divider');
    assert.strictEqual(menuItemsAndDividers.get(2).type, MenuItem, 'third child should be a MenuItem');
    assert.deepEqual(
      menuItemsAndDividers.get(1).props.style,
      {
        marginTop: 7,
        marginBottom: 8,
      },
      'the Divider gets default styles'
    );
  });

  describe('prop: children', () => {
    it("should merge the Divider's styles over the Menu's default divider styles", () => {
      const style = {
        color: 'red',
        marginTop: '999px',
      };
      const wrapper = shallowWithContext(
        <Menu>
          <Divider style={style} />
        </Menu>
      );

      const divider = wrapper.find(Divider);
      assert.strictEqual(divider.length, 1, 'there should be one divider child');

      assert.deepEqual(
        divider.props().style,
        Object.assign({}, style, {
          marginBottom: 8,
        }),
        "existing styles should be merged over Menu's styles"
      );
    });

    it('should be able to accept any children', () => {
      const child = <div foo="bar" />;
      const wrapper = shallowWithContext(
        <Menu>
          {child}
        </Menu>
      );
      assert.strictEqual(wrapper.contains(child), true);
    });
  });
});
